# ── i18n: shared language dictionary ─────────────────────────────────────────
# One dictionary for every user-visible string in HopDesk: the table engine,
# Panorama, Requisiciones and anything new. Spanish is the source language.
#
# Files (served as-is by Shiny, so R and the browser read the same text):
#   www/i18n/es.json   source of truth; every key MUST exist here
#   www/i18n/en.json   translation; a missing key falls back to Spanish
#
# Keys: "<area>.<screen>.<thing>", lowercase, ASCII, dots and underscores only.
#   areas: comun (shared verbs/nouns), tablas, panorama, requisiciones
#   e.g. "tablas.sumador.seleccionadas", "requisiciones.etapa.provisionada"
#
# Placeholders are named, never positional, so a translation can reorder them:
#   "{n} facturas seleccionadas"  ->  tr("tablas.sumador.n_sel", n = 3)
# Plurals use two sibling keys, "<key>.uno" and "<key>.otros"; tr_n() picks one.
#
# Rules:
#   - Never build a sentence out of fragments (paste0(tr("a"), " ", tr("b"))):
#     word order differs between languages. Make one key with placeholders.
#   - tr() returns plain text. htmltools tags escape it; if you paste it into
#     an HTML string yourself, escape it (htmltools::htmlEscape).
#   - A missing key never breaks the UI: tr() returns the key itself and logs
#     one warning per key, and tests/test_i18n.R fails on it.
#
# Browser side: i18n_head() puts the dictionary in the page and www/i18n.js
# exposes hdT(key, vars) and hdTn(key, n, vars) with the same rules.

.i18n_dir   <- file.path("www", "i18n")
.i18n_cache <- new.env(parent = emptyenv())
.i18n_warned <- new.env(parent = emptyenv())

i18n_idiomas <- function() c("es", "en")

i18n_dict <- function(idioma = "es") {
  if (!idioma %in% i18n_idiomas()) idioma <- "es"
  if (is.null(.i18n_cache[[idioma]])) {
    path <- file.path(.i18n_dir, paste0(idioma, ".json"))
    .i18n_cache[[idioma]] <- if (file.exists(path))
      jsonlite::fromJSON(path, simplifyVector = FALSE) else list()
  }
  .i18n_cache[[idioma]]
}

# Active language. One place to change it later (user setting, client default).
hd_idioma <- function(session = shiny::getDefaultReactiveDomain()) {
  getOption("hopdesk.idioma", "es")
}

tr <- function(clave, ..., idioma = hd_idioma()) {
  txt <- i18n_dict(idioma)[[clave]] %||% i18n_dict("es")[[clave]]
  if (is.null(txt)) {
    if (is.null(.i18n_warned[[clave]])) {
      .i18n_warned[[clave]] <- TRUE
      warning("i18n: missing key '", clave, "'", call. = FALSE)
    }
    return(clave)
  }
  vars <- list(...)
  for (nm in names(vars))
    txt <- gsub(paste0("{", nm, "}"), format(vars[[nm]], big.mark = ","), txt, fixed = TRUE)
  txt
}

tr_n <- function(clave, n, ..., idioma = hd_idioma()) {
  tr(paste0(clave, if (identical(as.numeric(n), 1)) ".uno" else ".otros"),
     n = n, ..., idioma = idioma)
}

# Dictionary for the browser: the active language merged over Spanish, so a
# missing translation still shows Spanish instead of a raw key.
i18n_head <- function(idioma = hd_idioma()) {
  dic <- utils::modifyList(i18n_dict("es"), i18n_dict(idioma))
  htmltools::tagList(
    htmltools::tags$script(htmltools::HTML(paste0(
      "window.HD_I18N=", jsonlite::toJSON(dic, auto_unbox = TRUE), ";"))),
    htmltools::tags$script(src = "i18n.js?v=1")
  )
}
