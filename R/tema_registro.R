# =============================================================================
# R/tema_registro.R
# HopDesk's design engine (TEMA): the one place that says what the app looks
# like. Approved design: hopdesk_coord/tablas/reportes/TEMA_paso0.md.
#
# Two layers of CSS custom properties, all named --hd-*:
#   paleta   the brand's constants (navy, ink, paper, gold...). The same in
#            every theme; www/hd_marca.css defines the same values under
#            .hd-marca and tests/test_temas_guardian.R keeps them equal.
#   roles    what a colour is FOR (fondo, primario, peligro...). Each theme
#            gives every role its value; screens use roles, never hex.
#   comunes  roles that are the same in every theme (tags A/B/C are a product
#            rule; type and spacing scales).
#
# www/hd_temas.css is GENERATED from this file by scripts/generar_temas_css.R
# and committed; nothing writes www/ at run time. The CSS puts the default
# theme on :root, so a page with no data-hd-tema attribute (or a script that
# never ran) still gets it, and the other themes on [data-hd-tema="..."].
# Emails, Word, Excel, PDF and SVG take resolved hex from tema_tokens(),
# because Gmail and Word do not understand var().
#
# Themes:
#   clasico  the bright-blue look the app has today (bslib flatly, #0A58CA).
#            Bootstrap keeps painting its own components with flatly (the
#            bridge in www/hd_tema_bootstrap.css applies under marca only).
#            Each role holds the most used hand-written value of its group
#            (TEMA_inventario.md §3), so moving a screen from hex to roles
#            keeps its look under Clásico.
#   marca    Panorama's look (navy + Fraunces), the official one. Default
#            only once Luis switches it on (TEMA stage 5).
#
# Loaded by shinyapps.io in alphabetical order before app.R (rule 29): only
# literal data and function definitions up here, no calls into other files.
# =============================================================================

HD_TEMA_OMISION <- "clasico"

# Bumped with every change to the generated file (browser cache on shinyapps.io).
HD_TEMAS_CSS_VERSION <- 1L

# ── Paleta (same in every theme) ─────────────────────────────────────────────
.HD_PALETA <- list(
  marino        = "#13213A",
  tinta         = "#1A2233",
  pizarra       = "#3B4558",
  gris          = "#5A6474",
  niebla        = "#B7C2D4",
  linea         = "#E3E8F0",
  papel         = "#F7F6F2",
  crema         = "#F3F1EA",
  blanco        = "#FFFFFF",
  oro           = "#C9AE6E",
  `oro-osc`     = "#B0924F",
  `oro-claro`   = "#E4CF9B",
  `oro-velo`    = "rgba(201, 174, 110, 0.14)",
  `ambar-bg`    = "#F7E9E1", `ambar-bd`  = "#E8CBBB", `ambar-tag`  = "#F0D6C8", `ambar-fg`  = "#9A4524",
  `salvia-bg`   = "#E2EEEF", `salvia-bd` = "#C6DDDF", `salvia-tag` = "#D0E5E7", `salvia-fg` = "#1E5B63",
  `lila-bg`     = "#F3E8EE", `lila-bd`   = "#E6CFDC", `lila-tag`   = "#EBD8E3", `lila-fg`   = "#6B3358",
  `olivo-bg`    = "#EEF0E2", `olivo-bd`  = "#DADFC6", `olivo-tag`  = "#E2E6CF", `olivo-fg`  = "#56602F",
  sube          = "#E6A383",
  baja          = "#A9C3D6",
  serif         = "'Fraunces', Georgia, 'Times New Roman', serif",
  sans          = "'Lato', system-ui, -apple-system, 'Segoe UI', sans-serif"
)

# ── Comunes (roles with one value in every theme) ────────────────────────────
.HD_COMUNES <- list(
  # Tags A (red), B (orange), C (yellow): pill text / pill / row (Luis'
  # rule; the same values as www/hdtable.css and HD_ETIQUETAS today).
  `etq-a-texto` = "#B42318", `etq-a-pastilla` = "#FFE1DE", `etq-a-fila` = "#FFF0F0",
  `etq-b-texto` = "#B54708", `etq-b-pastilla` = "#FFE8D1", `etq-b-fila` = "#FFF3E6",
  `etq-c-texto` = "#8A6100", `etq-c-pastilla` = "#FFF1B8", `etq-c-fila` = "#FFFBE6",
  # Type scale: absorbs today's 46 font sizes (TEMA_inventario.md §5).
  `txt-xs` = "11px", `txt-sm` = "12px", `txt-md` = "13px",
  `txt-base` = "14px", `txt-lg` = "16px", `txt-xl` = "20px",
  `esp-1` = "4px", `esp-2` = "8px", `esp-3` = "12px",
  `esp-4` = "16px", `esp-5` = "24px", `esp-6` = "32px",
  pildora = "999px"
)

# ── Temas (every theme gives every role a value) ─────────────────────────────
.HD_LATO <- "Lato, -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif"

HD_TEMAS <- list(
  clasico = list(
    nombre = "tema.clasico",
    tokens = list(
      # Values: the most used of each group, TEMA_inventario.md §3 (n).
      fondo              = "#FFFFFF",
      superficie         = "#FFFFFF",   # 137
      `superficie-2`     = "#F5F8FC",
      borde              = "#E5E7EB",   # 34
      `borde-fuerte`     = "#CCCCCC",   # 30
      texto              = "#374151",   # 16
      `texto-2`          = "#6B7280",   # 28
      `texto-tenue`      = "#9CA3AF",   # 13 (disabled, notes; not body text)
      `texto-inverso`    = "#FFFFFF",
      primario           = "#0A58CA",   # 54, bslib's primary
      `primario-hover`   = "#094BAC",   # Bootstrap's 15% shade of it
      `primario-texto`   = "#FFFFFF",
      `primario-suave`   = "#E8F0FE",
      acento             = "#B49444",   # 50, login and account emails
      `acento-texto`     = "#8B6914",
      enlace             = "#0A58CA",
      `barra-fondo`      = "#0A58CA",   # the navbar flatly paints today
      `barra-texto`      = "#FFFFFF",
      encabezado         = "#1E3A5F",   # 46, document and table headers (COBROS)
      `encabezado-texto` = "#FFFFFF",
      `encabezado-hover` = "#2A5080",
      `encabezado-borde` = "#163050",
      `pago-encabezado`  = "#3A1E5F",   # the Flujo's PAGOS header
      # A small dark surface: Flujo neto / Posición acumulada rows, Día 0, login.
      `oscuro-fondo`     = "#162D4A",
      `oscuro-fondo-2`   = "#112338",
      `oscuro-borde`     = "#0E1E30",
      `oscuro-texto`     = "#E8EEF5",
      `oscuro-texto-2`   = "#B0C4DE",
      `oscuro-exito`     = "#3FB950",   # positive / negative on the dark band (Bancos)
      `oscuro-peligro`   = "#F85149",
      # Fill (stripes, icons) and text are separate: a fill colour need not
      # read as text (TEMA_e4_mapas.md P8).
      exito              = "#198754", `exito-suave`   = "#D1E7DD", `exito-texto`   = "#065F46",
      aviso              = "#F59E0B", `aviso-suave`   = "#FFF3CD", `aviso-texto`   = "#856404",
      `aviso-fuerte`     = "#D97706",   # due today to the day after (Pasivos)
      peligro            = "#DC2626", `peligro-suave` = "#FEE2E2", `peligro-texto` = "#B91C1C",
      info               = "#0A58CA", `info-suave`    = "#E8F0FE",
      cobro              = "#1F5FD1",   # the table engine's (--hdt-cobro)
      pago               = "#15804A",   # the table engine's (--hdt-pago)
      provision          = "#7C3AED",   # 8
      seleccion          = "#D8E5FF",   # the table engine's (--hdt-sel)
      hover              = "#F0F4FF",   # the table engine's (--hdt-hover)
      `fuente-titulos` = .HD_LATO,
      `fuente-texto`   = .HD_LATO,
      `radio-sm`       = "0.25rem",
      `radio-md`       = "0.375rem",
      radio            = "0.5rem",
      sombra           = "0 0.5rem 1rem rgba(0, 0, 0, 0.15)",
      foco             = "0 0 0 0.25rem rgba(10, 88, 202, 0.25)",
      `foco-color`     = "#0A58CA"   # a focus outline or border (foco is a whole shadow)
    )
  ),
  marca = list(
    nombre = "tema.marca",
    tokens = list(
      fondo              = "#F7F6F2",   # papel
      superficie         = "#FFFFFF",
      `superficie-2`     = "#F3F1EA",   # crema
      borde              = "#D9D4C6",
      `borde-fuerte`     = "#B7C2D4",   # niebla
      texto              = "#1A2233",   # tinta
      `texto-2`          = "#5A6474",   # gris
      `texto-tenue`      = "#8B97AD",
      `texto-inverso`    = "#FFFFFF",
      primario           = "#13213A",   # marino
      `primario-hover`   = "#24365A",
      `primario-texto`   = "#FFFFFF",
      `primario-suave`   = "#E3E8F0",   # linea
      acento             = "#C9AE6E",   # oro: decoration and focus, never text
      `acento-texto`     = "#8A6D2C",   # gold that reads as text
      enlace             = "#1E5B63",
      `barra-fondo`      = "#13213A",
      `barra-texto`      = "#FFFFFF",
      encabezado         = "#13213A",
      `encabezado-texto` = "#FFFFFF",
      `encabezado-hover` = "#24365A",
      `encabezado-borde` = "#0B1527",
      `pago-encabezado`  = "#6B3358",   # lila-fg
      `oscuro-fondo`     = "#13213A",
      `oscuro-fondo-2`   = "#0E192C",
      `oscuro-borde`     = "#24365A",
      `oscuro-texto`     = "#F3F1EA",
      `oscuro-texto-2`   = "#B7C2D4",
      `oscuro-exito`     = "#9CCBA8",
      `oscuro-peligro`   = "#E6A383",   # Panorama's warm signal on the navy bar
      exito              = "#2F6B4F", `exito-suave`   = "#E3EFE8", `exito-texto`   = "#2F6B4F",
      aviso              = "#C08A3E", `aviso-suave`   = "#F7E9E1", `aviso-texto`   = "#9A4524",
      `aviso-fuerte`     = "#9A4524",
      peligro            = "#B42318", `peligro-suave` = "#FDECEA", `peligro-texto` = "#B42318",
      info               = "#1E5B63", `info-suave`    = "#E2EEEF",
      cobro              = "#24508F",
      pago               = "#2F6B4F",
      provision          = "#6B3358",   # lila-fg
      seleccion          = "#E3E8F0",
      hover              = "#F3F1EA",
      `fuente-titulos` = "'Fraunces', Georgia, 'Times New Roman', serif",
      `fuente-texto`   = "'Lato', system-ui, -apple-system, 'Segoe UI', sans-serif",
      `radio-sm`       = "12px",
      `radio-md`       = "18px",
      radio            = "24px",
      sombra           = "0 1px 2px rgba(19, 33, 58, 0.06), 0 8px 24px rgba(19, 33, 58, 0.06)",
      foco             = "0 0 0 3px rgba(201, 174, 110, 0.55)",
      `foco-color`     = "#8A6D2C"   # gold that a 1-2 px outline can show (oro is 2.15:1)
    )
  )
)

# ── Legado de Clásico ────────────────────────────────────────────────────────
# A screen moved from hex to roles (TEMA stage 4) unifies a colour with its
# role when they are close (ΔE <= 5). When today's colour is farther, the
# screen still uses the role, through a legacy token that keeps the exact
# value under Clásico only:
#     --hd-leg-<rol>-<hex>   e.g. --hd-leg-texto-0d1b2a
#     clasico -> #0D1B2A     every other theme -> var(--hd-texto)
# Usable in a .css, in an inline style = and from R (tema_color("leg-texto-
# 0d1b2a", tema) for Word, PDF and emails). Generated to
# www/hd_clasico_legado.css. When Clásico retires, scripts/
# retirar_legado_clasico.R turns every var(--hd-leg-ROL-hex) into
# var(--hd-ROL). The guardian ratchets the count. Entries: "<rol>-<hex>" =
# "#HEX" (the hex in the name, lower case, without #, is the value). For a
# shadow role (HD_ROLES_SOMBRA: a whole box-shadow, not a colour) the value
# is the whole shadow of today, the hex names its colour and a hash of the
# shadow (tema_hash_sombra()) tells apart two shadows of one colour:
#     `sombra-000000-<hash>` = "0 4px 20px rgba(0, 0, 0, 0.45)"
.HD_LEGADO <- list()

# Roles whose value is a whole box-shadow, not a colour.
HD_ROLES_SOMBRA <- c("sombra", "foco")

# Does a shadow use this colour (as #hex or as rgb()/rgba() of it)?
.tema_sombra_con_color <- function(sombra, hex) {
  rgb <- strtoi(substring(sub("^#", "", hex), c(1, 3, 5), c(2, 4, 6)), 16L)
  grepl(hex, sombra, ignore.case = TRUE, fixed = FALSE) ||
    grepl(sprintf("rgba?\\(\\s*%d\\s*,\\s*%d\\s*,\\s*%d\\s*[,)]", rgb[1], rgb[2], rgb[3]), sombra)
}

# "texto-2-6b7280" -> list(rol = "texto-2", hex = "#6B7280"); NULL if malformed.
# A shadow role's name also carries a short hash of the whole shadow, since
# one colour can have several shadows: "sombra-000000-3fa2c1"
# -> list(rol = "sombra", hex = "#000000", hash = "3fa2c1").
tema_legado_partes <- function(nombre) {
  m <- regmatches(nombre, regexec("^(.+?)-([0-9a-f]{6})(?:-([0-9a-f]{6}))?$", nombre, perl = TRUE))[[1]]
  if (length(m) != 4L) return(NULL)
  list(rol = m[2], hex = paste0("#", toupper(m[3])), hash = if (nzchar(m[4])) m[4] else NULL)
}

# The hash in a legacy shadow's name: the first 6 hex of the MD5 of the
# shadow, lower case and without spaces ("0 4px 20px rgba(0,0,0,0.45)").
tema_hash_sombra <- function(valor) {
  f <- tempfile(); on.exit(unlink(f))
  writeBin(charToRaw(tolower(gsub("\\s+", "", valor))), f)
  substr(unname(tools::md5sum(f)), 1L, 6L)
}

# The value of every legacy token under a theme.
.tema_legado_valores <- function(tema) {
  if (!length(.HD_LEGADO)) return(list())
  if (identical(tema, "clasico")) return(.HD_LEGADO)
  roles <- HD_TEMAS[[tema]]$tokens
  stats::setNames(lapply(names(.HD_LEGADO), function(n) roles[[tema_legado_partes(n)$rol]]),
                  names(.HD_LEGADO))
}

# One line of code with the legacy tokens turned into their roles: what
# scripts/retirar_legado_clasico.R applies to every file.
tema_legado_retirar <- function(lineas) {
  lineas <- gsub("var\\(--hd-leg-([a-z0-9-]+?)-[0-9a-f]{6}(?:-[0-9a-f]{6})?\\)", "var(--hd-\\1)", lineas, perl = TRUE)
  gsub("([\"'])leg-([a-z0-9-]+?)-[0-9a-f]{6}(?:-[0-9a-f]{6})?([\"'])", "\\1\\2\\3", lineas, perl = TRUE)
}

# A role (or legacy token) at N % over transparent, for what cannot take
# var() or color-mix(): SVG in a data: URI, vis.js canvas, Word, Excel. The
# same colour as color-mix(in srgb, var(--hd-rol) N%, transparent):
#   tema_mezcla("provision", 25)                 "rgba(124, 58, 237, 0.25)"
# `sobre`: a solid background to flatten onto, for places with no alpha
# (Word, Excel, emails): tema_mezcla("provision", 25, sobre = "#FFFFFF").
tema_mezcla <- function(rol, pct, tema = HD_TEMA_OMISION, sobre = NULL) {
  hex <- tema_color(rol, tema)
  if (!grepl("^#[0-9A-Fa-f]{6}$", hex)) stop("[tema] tema_mezcla needs a #rrggbb role: ", rol, call. = FALSE)
  a <- max(0, min(100, pct)) / 100
  rgb <- strtoi(substring(hex, c(2, 4, 6), c(3, 5, 7)), 16L)
  if (is.null(sobre)) return(sprintf("rgba(%d, %d, %d, %s)", rgb[1], rgb[2], rgb[3], format(a)))
  f <- strtoi(substring(sobre, c(2, 4, 6), c(3, 5, 7)), 16L)
  m <- as.integer(round(rgb * a + f * (1 - a)))
  sprintf("#%02X%02X%02X", m[1], m[2], m[3])
}

# ── Lectura ──────────────────────────────────────────────────────────────────

temas_disponibles <- function() names(HD_TEMAS)

tema_valido <- function(tema) {
  t <- tolower(trimws(as.character(tema %||% "")))[1]
  !is.na(t) && t %in% names(HD_TEMAS)
}

# The theme a person sees: theirs, else their client's, else the system's.
# Anything unknown (a theme removed later, a failed read passed as NULL) falls
# through; it never errors.
tema_resolver <- function(usuario = NULL, cliente = NULL) {
  for (t in list(usuario, cliente)) if (tema_valido(t)) return(tolower(trimws(t)))
  HD_TEMA_OMISION
}

# The theme of a Shiny session, for colours drawn in R (SVG in data: URIs,
# vis.js, Word...). TEMA stage 3 puts a reactiveVal in
# session$userData$hd_tema; read inside a render, a change of theme redraws.
# Until then, and outside a session, the default theme.
tema_de_sesion <- function(session = shiny::getDefaultReactiveDomain()) {
  rv <- if (is.null(session)) NULL else session$userData$hd_tema
  tema_resolver(if (is.function(rv)) rv() else NULL)
}

# Every --hd-* value of a theme, resolved: paleta + comunes + its roles +
# its legacy tokens (as "leg-<rol>-<hex>").
tema_tokens <- function(tema = HD_TEMA_OMISION) {
  if (!tema_valido(tema)) tema <- HD_TEMA_OMISION
  tema <- tolower(trimws(tema))
  leg <- .tema_legado_valores(tema)
  if (length(leg)) names(leg) <- paste0("leg-", names(leg))
  c(.HD_PALETA, .HD_COMUNES, HD_TEMAS[[tema]]$tokens, leg)
}

tema_color <- function(rol, tema = HD_TEMA_OMISION) {
  v <- tema_tokens(tema)[[rol]]
  if (is.null(v)) stop("[tema] rol desconocido: ", rol, call. = FALSE)
  v
}

# ── CSS ──────────────────────────────────────────────────────────────────────

.tema_bloque <- function(selector, valores, titulo = NULL) {
  c(if (!is.null(titulo)) paste0("/* ", titulo, " */"),
    paste0(selector, " {"),
    paste0("  --hd-", names(valores), ": ", unlist(valores, use.names = FALSE), ";"),
    "}")
}

# The text of www/hd_temas.css (scripts/generar_temas_css.R writes it).
tema_css <- function() {
  otros <- setdiff(names(HD_TEMAS), HD_TEMA_OMISION)
  lineas <- c(
    "/* ============================================================================",
    "   www/hd_temas.css: GENERATED from R/tema_registro.R by",
    "   scripts/generar_temas_css.R. Do not edit by hand.",
    paste0("   Default theme on :root: ", HD_TEMA_OMISION, ". The others on <html data-hd-tema=\"...\">."),
    "   ========================================================================== */",
    "",
    .tema_bloque(":root", .HD_PALETA, "Paleta: the same in every theme"),
    "",
    .tema_bloque(":root", .HD_COMUNES, "Comunes: the same in every theme"),
    "",
    .tema_bloque(paste0(":root, [data-hd-tema=\"", HD_TEMA_OMISION, "\"]"),
                 HD_TEMAS[[HD_TEMA_OMISION]]$tokens,
                 paste0("Roles: ", HD_TEMA_OMISION, " (default)"))
  )
  for (t in otros)
    lineas <- c(lineas, "", .tema_bloque(paste0("[data-hd-tema=\"", t, "\"]"),
                                         HD_TEMAS[[t]]$tokens, paste0("Roles: ", t)))
  c(lineas, "")
}

# The text of www/hd_clasico_legado.css. Every theme but Clásico gets the
# role; Clásico (also on :root while it is the default) gets today's hex.
tema_legado_css <- function() {
  cab <- c(
    "/* ============================================================================",
    "   www/hd_clasico_legado.css: GENERATED from .HD_LEGADO in R/tema_registro.R",
    "   by scripts/generar_temas_css.R. Do not edit by hand.",
    "   --hd-leg-<rol>-<hex>: today's exact colour under Clasico, the role elsewhere.",
    "   Can only shrink (tests/test_temas_guardian.R).",
    "   ========================================================================== */")
  if (!length(.HD_LEGADO)) return(c(cab, "", "/* (empty) */", ""))
  a_rol <- stats::setNames(lapply(names(.HD_LEGADO), function(n)
    paste0("var(--hd-", tema_legado_partes(n)$rol, ")")), paste0("leg-", names(.HD_LEGADO)))
  clasico <- stats::setNames(.HD_LEGADO, paste0("leg-", names(.HD_LEGADO)))
  sel <- "[data-hd-tema=\"clasico\"]"
  if (identical(HD_TEMA_OMISION, "clasico")) sel <- paste0(sel, ", :root:not([data-hd-tema])")
  c(cab, "",
    .tema_bloque(":root", a_rol, "Every theme: the role"), "",
    .tema_bloque(sel, clasico, "Clasico: today's exact value"), "")
}

# ── Para app.R ───────────────────────────────────────────────────────────────

# The one Bootstrap theme of every page (it was written three times in app.R).
# No per-session Sass: themes switch through the CSS custom properties.
hd_bs_theme <- function() {
  bslib::bs_theme(version = 5, bootswatch = "flatly",
                  primary = HD_TEMAS$clasico$tokens$primario)
}

# Goes in a page's header: the generated tokens and the theme attribute.
# tags$head() is hoisted into <head>, so the attribute is set while the page
# parses, before anything paints, and no server message is involved: nothing
# here can hold up the start. The CSS does not depend on it either (:root
# carries the default theme).
tema_head <- function() {
  htmltools::tags$head(
    htmltools::tags$link(rel = "stylesheet", type = "text/css",
                         href = paste0("hd_temas.css?v=", HD_TEMAS_CSS_VERSION)),
    htmltools::tags$link(rel = "stylesheet", type = "text/css",
                         href = paste0("hd_clasico_legado.css?v=", HD_TEMAS_CSS_VERSION)),
    htmltools::tags$script(htmltools::HTML(paste0(
      "document.documentElement.setAttribute('data-hd-tema','", HD_TEMA_OMISION, "');")))
  )
}
