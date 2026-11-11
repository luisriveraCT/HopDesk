# =============================================================================
# tests/helpers/avisos_guardiana.R
# Shared checks for the "notices come from the dictionary" guards
# (tests/test_<area>_avisos_r.R, avisos-varios; same rule as
# tests/test_ledger_avisos_r.R):
#   avisos_fijos(f)        the showNotification() calls in f whose message
#                          (first argument) is fixed text: a literal with
#                          words, or a paste/paste0/sprintf holding one;
#   avisos_claves(f, pre)  the keys starting with `pre` that f uses;
#   clave_en_es_y_en(k)    k exists in both dictionaries.
# The caller sources R/i18n.R first.
# =============================================================================
.tiene_palabras <- function(x) is.character(x) && length(x) == 1L && grepl("[[:alpha:]]{3,}", x)

avisos_fijos <- function(f) {
  fijos <- character(0)
  recorrer <- function(x) {
    if (!is.call(x)) return(invisible())
    if (identical(x[[1]], as.name("showNotification")) ||
        (is.call(x[[1]]) && identical(x[[1]][[1]], as.name("::")) && identical(x[[1]][[3]], as.name("showNotification")))) {
      if (length(x) >= 2L) {
        m <- x[[2]]
        if (.tiene_palabras(m) ||
            (is.call(m) && as.character(m[[1]])[1] %in% c("paste", "paste0", "sprintf") &&
             any(vapply(as.list(m)[-1], .tiene_palabras, logical(1)))))
          fijos <<- c(fijos, paste(deparse(m, width.cutoff = 120L)[1]))
      }
    }
    xs <- as.list(x); for (k in seq_along(xs)) if (is.call(xs[[k]])) recorrer(xs[[k]])
  }
  for (e in parse(f, keep.source = FALSE, encoding = "UTF-8")) recorrer(e)
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
