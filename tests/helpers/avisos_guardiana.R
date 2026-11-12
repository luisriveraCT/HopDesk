# =============================================================================
# tests/helpers/avisos_guardiana.R
# Shared checks for the "notices come from the dictionary" guards
# (tests/test_<area>_avisos_r.R, avisos-varios; same rule as
# tests/test_ledger_avisos_r.R):
#   avisos_fijos(f)        the showNotification() calls in f whose message
#                          (first argument) is fixed text: a literal with
#                          words, a paste/paste0/sprintf holding one, or an
#                          if/else with one in a branch (.es_fijo);
#   avisos_claves(f, pre)  the keys starting with `pre` that f uses;
#   clave_en_es_y_en(k)    k exists in both dictionaries.
# The caller sources R/i18n.R first.
# =============================================================================
.tiene_palabras <- function(x) is.character(x) && length(x) == 1L && grepl("[[:alpha:]]{3,}", x)

# A message is fixed text when it is a literal with words, a
# paste/paste0/sprintf holding one, or an if/else with such a message in any
# branch: showNotification(if (ok) "Guardado." else "Error.") was invisible
# to the guards until avisos-integrador (2026-11-11), and 6 of them were left
# in R/ and app.R. Parentheses around the message are looked through.
.es_fijo <- function(m) {
  if (.tiene_palabras(m)) return(TRUE)
  if (!is.call(m)) return(FALSE)
  f <- as.character(m[[1]])[1]
  if (f %in% c("paste", "paste0", "sprintf"))
    return(any(vapply(as.list(m)[-1], .tiene_palabras, logical(1))))
  if (f == "if") return(any(vapply(as.list(m)[-(1:2)], .es_fijo, logical(1))))
  if (f == "(") return(.es_fijo(m[[2]]))
  FALSE
}

# Fixed text can also come in through a variable: `x <- "Cobros"` (or an
# if/else of literals) and then showNotification(x) or tr("clave", p = x).
# A name counts within the function that assigns it fixed text, not in other
# functions of the file (textos-codigo, 2026-11-12: search_module.R's ledger
# name reached the Lupa's notice so; the guards did not see it).
# Only names whose every assignment there is fixed text (one that builds on
# itself, msg <- paste0(msg, ...), does not count against it): a name that
# also takes an input or a computed value is not taken for fixed text.
.variables_fijas <- function(ex) {
  fijos <- character(0); otros <- character(0)
  ver <- function(x) {
    if (!is.call(x)) return(invisible())
    if (identical(x[[1]], as.name("function"))) return(invisible())   # its own scope
    if (as.character(x[[1]])[1] %in% c("<-", "=") && length(x) == 3L && is.name(x[[2]])) {
      n <- as.character(x[[2]])
      if (.es_fijo(x[[3]])) fijos <<- c(fijos, n)
      else if (!n %in% all.names(x[[3]])) otros <<- c(otros, n)
    }
    xs <- as.list(x); for (k in seq_along(xs)) if (is.call(xs[[k]])) ver(xs[[k]])
  }
  for (e in ex) ver(e)
  setdiff(unique(fijos), otros)
}

avisos_fijos <- function(f) {
  ex <- parse(f, keep.source = FALSE, encoding = "UTF-8")
  fijas <- .variables_fijas(ex)   # the file's top level; each function swaps in its own
  de_variable <- function(a) is.name(a) && as.character(a) %in% fijas
  fijo <- function(m) {
    if (.es_fijo(m) || de_variable(m)) return(TRUE)
    # tr("clave", p = <fixed text or a variable holding it>)
    if (is.call(m) && as.character(m[[1]])[1] %in% c("tr", "tr_n") && length(m) > 2L)
      return(any(vapply(as.list(m)[-(1:2)], function(a) .es_fijo(a) || de_variable(a), logical(1))))
    if (is.call(m) && identical(as.character(m[[1]])[1], "if"))
      return(any(vapply(as.list(m)[-(1:2)], fijo, logical(1))))
    FALSE
  }
  fijos <- character(0)
  recorrer <- function(x) {
    if (!is.call(x)) return(invisible())
    if (identical(x[[1]], as.name("function")) && length(x) >= 3L) {
      antes <- fijas; on.exit(fijas <<- antes)
      fijas <<- .variables_fijas(list(x[[3]]))
      recorrer(x[[3]])
      return(invisible())
    }
    if (identical(x[[1]], as.name("showNotification")) ||
        (is.call(x[[1]]) && identical(x[[1]][[1]], as.name("::")) && identical(x[[1]][[3]], as.name("showNotification")))) {
      if (length(x) >= 2L && fijo(x[[2]]))
        fijos <<- c(fijos, paste(deparse(x[[2]], width.cutoff = 120L)[1]))
    }
    xs <- as.list(x); for (k in seq_along(xs)) if (is.call(xs[[k]])) recorrer(xs[[k]])
  }
  for (e in ex) recorrer(e)
  fijos
}

avisos_claves <- function(f, pre) {
  # By bytes: with a non-UTF-8 locale (Windows), character offsets after the
  # first accent would cut the keys short.
  codigo <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  patron <- paste0(gsub(".", "\\.", pre, fixed = TRUE), "[a-z0-9_]+")
  unique(regmatches(codigo, gregexpr(patron, codigo, useBytes = TRUE))[[1]])
}

clave_en_es_y_en <- function(k) !is.null(i18n_dict("es")[[k]]) && !is.null(i18n_dict("en")[[k]])

# ── Added by settings-avisos ─────────────────────────────────────────────────
#   avisos_claves_n(f, pre)       avisos_claves(), but a key passed to tr_n()
#                                 counts as its two siblings, .uno and .otros;
#   avisos_claves_faltan(claves)  those missing from es or en;
#   avisos_distintos(iguales)     the list(now, before) pairs that differ.
avisos_claves_n <- function(f, pre) {
  codigo <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  patron <- paste0('tr_n\\("', gsub(".", "\\.", pre, fixed = TRUE), '[a-z0-9_]+"')
  pl <- unique(regmatches(codigo, gregexpr(patron, codigo, useBytes = TRUE))[[1]])
  pl <- sub('^tr_n\\("', "", sub('"$', "", pl))
  # paste0(character(0), ...) would give ".uno"/".otros" alone: only when there are plurals.
  c(setdiff(avisos_claves(f, pre), pl), if (length(pl)) paste0(rep(pl, each = 2L), c(".uno", ".otros")))
}

avisos_claves_faltan <- function(claves) claves[!vapply(claves, clave_en_es_y_en, logical(1))]

avisos_distintos <- function(iguales) Filter(function(p) !identical(p[[1]], p[[2]]), iguales)

# ── Added by fix/pasivos-asistente-fecha ─────────────────────────────────────
#   avisos_errores_fijos(f)  fixed text put into an error list -- errors$x <-
#                            "...", errors[["x"]] <- paste0("...", ...), any
#                            list whose name has "err" -- which is how the
#                            validators (pasivos_wizard_validate.R) give their
#                            messages: avisos_fijos() never sees them, they are
#                            not showNotification().
#                            A literal that IS a dictionary key (it exists in
#                            es.json) is not fixed text: the screen passes it
#                            to tr() (T31: cuentas.error.*). A made-up key is.
.clave_del_diccionario <- function(x) {
  if (!is.character(x) || length(x) != 1L || !grepl("^[a-z]+(\\.[a-z0-9_]+)+$", x)) return(FALSE)
  if (is.null(.avisos_dic_es$claves))
    .avisos_dic_es$claves <- names(jsonlite::fromJSON(file.path("www", "i18n", "es.json")))
  x %in% .avisos_dic_es$claves
}
.avisos_dic_es <- new.env()
avisos_errores_fijos <- function(f) {
  es_lista_errores <- function(lhs) is.call(lhs) && as.character(lhs[[1]])[1] %in% c("$", "[[") &&
    is.name(lhs[[2]]) && grepl("err", as.character(lhs[[2]]), ignore.case = TRUE)
  fijos <- character(0)
  recorrer <- function(x) {
    if (!is.call(x)) return(invisible())
    if (as.character(x[[1]])[1] %in% c("<-", "=") && length(x) == 3L && es_lista_errores(x[[2]])) {
      m <- x[[3]]
      if ((.tiene_palabras(m) && !.clave_del_diccionario(m)) ||
          (is.call(m) && as.character(m[[1]])[1] %in% c("paste", "paste0", "sprintf") &&
           any(vapply(as.list(m)[-1], .tiene_palabras, logical(1)))))
        fijos <<- c(fijos, paste(deparse(m, width.cutoff = 120L)[1]))
    }
    xs <- as.list(x); for (k in seq_along(xs)) if (is.call(xs[[k]])) recorrer(xs[[k]])
  }
  for (e in parse(f, keep.source = FALSE, encoding = "UTF-8")) recorrer(e)
  fijos
}
