# ── i18n: shared language dictionary ─────────────────────────────────────────
# One dictionary for every user-visible string in HopDesk: the table engine,
# Panorama, Requisiciones and anything new. Spanish is the source language.
#
# Files (served as-is by Shiny, so R and the browser read the same text):
#   www/i18n/es.json   source of truth; every key MUST exist here
#   www/i18n/en.json   translation; a missing key falls back to Spanish
#
# Keys: "<area>.<screen>.<thing>", lowercase, ASCII, dots and underscores only.
#   areas: comun (shared verbs/nouns), tablas, panorama, requisiciones,
#   semana (the week's cards, shared by Panorama and the weekly emails, T45)
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
# Only the keys under `prefijo`, for a page that does not carry the whole
# dictionary (the sign-in page: the loading overlay's texts, cuentas.carga.*,
# www/overlay_init.js). Merged into window.HD_I18N, which i18n_head() later
# replaces with the whole one.
i18n_head_parcial <- function(prefijo, idioma = hd_idioma()) {
  dic <- utils::modifyList(i18n_dict("es"), i18n_dict(idioma))
  dic <- dic[startsWith(names(dic), prefijo)]
  # "</" escaped: a text holding "</script>" would end the <script> early
  # (jsonlite does not escape it; "<\/" is the same string to JavaScript).
  json <- gsub("</", "<\\/", as.character(jsonlite::toJSON(dic, auto_unbox = TRUE)), fixed = TRUE)
  htmltools::tags$script(htmltools::HTML(paste0(
    "window.HD_I18N=Object.assign(window.HD_I18N||{},", json, ");")))
}

# T87-M1: keys that never travel to a page -- the names of HopDesk's internal
# permissions and of the staff tiers. The page's <head> is built before
# anyone signs in, so it is the same for everyone: they are left out for all.
# R still has them (tr() reads the dictionary on the server); no JavaScript
# asks for them (tests/test_t87_m125.R checks every hdT("...") of www/).
# From the registries (R/permisos.R, R/tier_registry.R), never a list here.
i18n_claves_internas <- function() {
  c(if (exists("permisos_internos", mode = "function")) paste0("permisos.", permisos_internos()),
    if (exists("niveles_staff", mode = "function")) paste0("niveles.nombre.", niveles_staff()))
}

# T118: the page carries only the keys its JavaScript asks for. tr() runs on
# the server, so everything R draws (every staff screen included) arrives
# already translated, and only to whoever it is drawn for; the browser needs
# the dictionary only for hdT()/hdTn(). Taken from the code, never a list:
#   - every quoted literal of www/*.js that is a key (or a plural base: its
#     .uno/.otros), ternaries and defaults included;
#   - in R/ and app.R, the keys of the JavaScript they carry: hdT('…'),
#     hdTn('…'), t('…'), tn('…') with a single-quoted key;
#   - the families a script builds at run time ('comun.mes.' + k,
#     'cuentas.carga.mensaje.' + n): a quoted literal ending in "." and
#     followed by "+" brings every key under that prefix.
# A key a table passes to the engine (its counter, its confirmation, its
# pinned label) is not here: it travels with that table's data
# (i18n_textos_navegador(), R/hd_table.R), so a staff table's words only
# reach staff. tests/test_t118_diccionario_audiencia.R guards the call sites.
.i18n_literales <- function(x, patron) {
  m <- regmatches(x, gregexpr(patron, x, perl = TRUE))[[1]]
  if (!length(m)) character() else sub(patron, "\\1", m, perl = TRUE)
}

i18n_claves_navegador <- function(raiz = ".") {
  if (!is.null(.i18n_cache$.navegador)) return(.i18n_cache$.navegador)
  k <- names(i18n_dict("es"))
  leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  js <- list.files(file.path(raiz, "www"), pattern = "[.]js$", full.names = TRUE)
  js <- js[basename(js) != "i18n.js"]
  rs <- c(list.files(file.path(raiz, "R"), pattern = "[.]R$", full.names = TRUE), file.path(raiz, "app.R"))
  lits <- c(
    unlist(lapply(js, function(f) .i18n_literales(leer(f), "['\"]([a-z][A-Za-z0-9_]*(?:[.][A-Za-z0-9_]+)+)['\"]"))),
    unlist(lapply(rs[file.exists(rs)], function(f)
      .i18n_literales(leer(f), "(?:\\bhdTn?|(?<![A-Za-z0-9_.$])tn?)\\(\\s*'([a-z][A-Za-z0-9_]*(?:[.][A-Za-z0-9_]+)+)'"))))
  lits <- unique(lits)
  familias <- unique(unlist(lapply(js, function(f)
    .i18n_literales(leer(f), "['\"]([a-z][A-Za-z0-9_]*(?:[.][A-Za-z0-9_]+)*[.])['\"]\\s*\\+"))))
  fam <- k[Reduce(`|`, lapply(familias, function(p) startsWith(k, p)), rep(FALSE, length(k)))]
  r <- sort(setdiff(intersect(unique(c(lits, paste0(lits, ".uno"), paste0(lits, ".otros"), fam)), k),
                    i18n_claves_internas()))
  .i18n_cache$.navegador <- r
  r
}

# The words a table hands the engine by key (bases: plurals bring .uno and
# .otros), in the session's language over Spanish, to travel with its data.
i18n_textos_navegador <- function(bases, idioma = hd_idioma()) {
  bases <- unique(as.character(unlist(bases)))
  bases <- bases[!is.na(bases) & nzchar(bases)]
  dic <- utils::modifyList(i18n_dict("es"), i18n_dict(idioma))
  ks <- setdiff(intersect(unique(c(bases, paste0(bases, ".uno"), paste0(bases, ".otros"))), names(dic)),
                i18n_claves_internas())
  dic[ks]
}

i18n_head <- function(idioma = hd_idioma()) {
  dic <- utils::modifyList(i18n_dict("es"), i18n_dict(idioma))
  dic <- dic[intersect(names(dic), i18n_claves_navegador())]
  # "</" escaped, as in i18n_head_parcial(): a text holding "</script>" would
  # end the <script> early.
  json <- gsub("</", "<\\/", as.character(jsonlite::toJSON(dic, auto_unbox = TRUE)), fixed = TRUE)
  htmltools::tagList(
    htmltools::tags$script(htmltools::HTML(paste0("window.HD_I18N=", json, ";"))),
    htmltools::tags$script(src = "i18n.js?v=1")
  )
}
