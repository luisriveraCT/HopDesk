# =============================================================================
# tests/test_temas_guardian.R
# Guard for the design engine (TEMA, R/tema_registro.R). Fails when:
#   1. a file gains hand-written colours (hex, rgb(), hsl()) outside a line
#      that defines a CSS custom property. Ratchet: each file may stay or go
#      DOWN; lower .TRINQUETE in the same commit that migrates a file. A file
#      not in the table starts at 0;
#   2. a file listed in .EXENTOS has no reason, or is missing;
#   3. the themes do not define the same roles, or a name is defined twice
#      across paleta / comunes / roles;
#   4. CSS or R uses a var(--hd-*) that nothing defines (a typo fails silently
#      in CSS);
#   5. www/hd_temas.css differs from what the registry generates, or
#      www/hd_marca.css disagrees with the registry on a value;
#   6. a text pair fails WCAG AA (4.5:1) in Marca, or in Clásico one of the
#      pairs that pass today (some of Clásico's do not; TEMA_inventario.md §2);
#   7. R/tema_registro.R does anything at the top other than define literal
#      data and functions (rule 29: shinyapps.io loads R/ before app.R), or
#      hd_bs_theme() stops being the theme app.R had (identical()).
#   8. Clásico's legacy tokens break their rules (see the section);
#   9. the Bootstrap bridge (www/hd_tema_bootstrap.css) differs from the
#      registry, has a rule outside html[data-hd-tema="<theme with bridge>"]
#      (Clásico has none), a colour written by hand, a triplet off its role,
#      or paints a text pair below AA.
#
# Run from project root:  Rscript tests/test_temas_guardian.R
# =============================================================================
cat("=== test_temas_guardian ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

tema <- new.env()
sys.source("R/tema_registro.R", envir = tema)

RE_HEX <- "(?<![&\\w])#(?:[0-9a-fA-F]{8}|[0-9a-fA-F]{6}|[0-9a-fA-F]{3,4})(?![0-9a-zA-Z_-])"
RE_FUN <- "\\b(?:rgba?|hsla?)\\("
RE_DEF <- "^\\s*--[a-zA-Z0-9-]+\\s*:"
cuenta <- function(x, re) sum(lengths(regmatches(x, gregexpr(re, x, perl = TRUE))))
leer   <- function(f) readLines(f, warn = FALSE, encoding = "UTF-8")
colores_a_mano <- function(f) {
  x <- leer(f)
  x <- x[!grepl(RE_DEF, x, perl = TRUE)]
  cuenta(x, RE_HEX) + cuenta(x, RE_FUN)
}
archivos <- c("app.R",
  list.files("R",   pattern = "\\.R$",          recursive = TRUE, full.names = TRUE),
  list.files("www", pattern = "\\.(css|js)$",   recursive = TRUE, full.names = TRUE))

# ── 1. Ratchet ───────────────────────────────────────────────────────────────
# Counts on master e2d3882 (app.R already without its three bs_theme copies).
.TRINQUETE <- c(
  "app.R" = 99L,
  "R/audit_log_viewer_module.R" = 17L,
  "R/audit_sticky_ui.R" = 4L,
  "R/auth_reset.R" = 3L,
  "R/avisos_correos.R" = 32L,
  "R/bancos_module.R" = 62L,          # TEMA-Bancos: 41 dead rules of bancos_styles() gone (-50)
  "R/cashflow_export_module.R" = 203L,
  "R/cashflow_preview_module.R" = 6L,
  "R/email_service.R" = 79L,
  "R/empresas_module.R" = 8L,
  "R/forecasting_module.R" = 2L,
  "R/global.R" = 16L,
  "R/hd_columnas.R" = 9L,
  "R/ledger_module.R" = 3L,
  "R/pagos_audit_viewer_module.R" = 7L,
  "R/pasivos_table_module.R" = 3L,
  "R/pasivos_table_styles.R" = 4L,    # stage 4: only the currency badges (.PASIVOS_CUR_COLORES), with TEMA 4b
  "R/pasivos_wizard_module.R" = 2L,
  "R/pasivos_wizard_steps.R" = 1L,
  "R/proveedores_module.R" = 1L,
  "R/reporte_module.R" = 157L,
  "R/requisiciones_persistence.R" = 1L,
  "R/settings/settings_hub.R" = 3L,
  "R/settings/settings_policies.R" = 20L,
  "R/staging_browse_module.R" = 14L,
  "R/tier_registry.R" = 8L,
  "R/tiers_module.R" = 36L,
  "R/treasury_map_module.R" = 4L,    # stage 4: only the 4 transparent #00000000 of vis.js (exempt)
  "R/ui_components.R" = 230L,         # TEMA-P1 (-2), then TEMA 2b: the top bar to barra-texto (-5)
  "www/hdtable.css" = 23L,
  "www/overlay_init.js" = 15L,
  "www/panorama.css" = 4L,
  "www/requisiciones.css" = 28L
)

# ── 2. Exempt files, each with its reason ────────────────────────────────────
.EXENTOS <- c(
  "R/tema_registro.R"   = "the registry: where every colour lives",
  "www/hd_temas.css"    = "generated from the registry",
  "R/bancos_formatos.R" = "BanBajío's Excel layout: a bank format, not the app's look",
  "R/tema_contraste.R"  = "the contrast checker: its colours are worked examples in its comments",
  "www/hd_clasico_legado.css" = "generated from the registry's legacy tokens",
  "www/hd_tema_bootstrap.css" = "generated from the registry's Bootstrap bridge (.HD_PUENTE_BS)"
)

for (f in setdiff(archivos, names(.EXENTOS))) {
  n <- colores_a_mano(f)
  tope <- if (f %in% names(.TRINQUETE)) .TRINQUETE[[f]] else 0L
  if (n > 0L || tope > 0L)
    ok(sprintf("%s: %d hand-written colours (ratchet %d)", f, n, tope), n <= tope)
}
ok("ratchet lists only existing files",
   all(file.exists(names(.TRINQUETE))) ||
     { message("  missing: ", paste(names(.TRINQUETE)[!file.exists(names(.TRINQUETE))], collapse = ", ")); FALSE })
ok("every exempt file exists and says why",
   all(file.exists(names(.EXENTOS))) && all(nzchar(trimws(.EXENTOS))))

# ── 3. Same roles in every theme, no name defined twice ──────────────────────
roles <- lapply(tema$HD_TEMAS, function(t) names(t$tokens))
ok("every theme defines the same roles",
   all(vapply(roles, function(r) setequal(r, roles[[1]]), logical(1))))
ok("every theme has a dictionary key for its name",
   all(vapply(tema$HD_TEMAS, function(t) grepl("^tema\\.", t$nombre %||% ""), logical(1))))
todas <- c(names(tema$.HD_PALETA), names(tema$.HD_COMUNES), roles[[1]],
           if (length(tema$.HD_LEGADO)) paste0("leg-", names(tema$.HD_LEGADO)))
ok("no token is defined twice (paleta / comunes / roles)", !anyDuplicated(todas))
ok("the default theme exists", tema$HD_TEMA_OMISION %in% names(tema$HD_TEMAS))
ok("every value is one non-empty string",
   all(vapply(c(tema$.HD_PALETA, tema$.HD_COMUNES, unlist(lapply(tema$HD_TEMAS, `[[`, "tokens"), recursive = FALSE)),
              function(v) is.character(v) && length(v) == 1L && nzchar(v), logical(1))))

# ── 4. No var(--hd-*) without a definition ──────────────────────────────────
definidos <- unique(c(todas, unlist(lapply(archivos, function(f)
  sub("^--hd-", "", unlist(regmatches(leer(f), gregexpr("--hd-[a-zA-Z0-9-]+(?=\\s*:)", leer(f), perl = TRUE))))))))
usados <- unlist(lapply(archivos, function(f) {
  x <- leer(f)
  if (grepl("\\.R$", f)) x <- x[!grepl("^\\s*#", x)]   # an R comment may quote a pattern
  m <- unlist(regmatches(x, gregexpr("var\\(--hd-[a-zA-Z0-9-]+", x, perl = TRUE)))
  if (length(m)) paste0(f, ": ", sub("^var\\(--hd-", "", m)) else character()
}))
huerfanos <- usados[!sub("^.*: ", "", usados) %in% definidos]
ok("every var(--hd-*) has a definition",
   length(huerfanos) == 0L || { message("  undefined: ", paste(unique(huerfanos), collapse = "; ")); FALSE })

# ── 5. The generated file and hd_marca.css agree with the registry ──────────
sin_fin <- function(x) { x <- sub("\r$", "", x); while (length(x) && !nzchar(x[length(x)])) x <- x[-length(x)]; x }
ok("www/hd_temas.css is what scripts/generar_temas_css.R generates",
   identical(sin_fin(leer("www/hd_temas.css")), sin_fin(tema$tema_css())))
marca_css <- paste(leer("www/hd_marca.css"), collapse = "\n")
pares <- regmatches(marca_css, gregexpr("--hd-([a-zA-Z0-9-]+)\\s*:\\s*([^;]+);", marca_css, perl = TRUE))[[1]]
nombres <- sub("^--hd-([a-zA-Z0-9-]+)\\s*:.*$", "\\1", pares)
valores <- trimws(sub("^--hd-[a-zA-Z0-9-]+\\s*:\\s*([^;]+);$", "\\1", pares))
marca_reg <- tema$tema_tokens("marca")
norm <- function(v) tolower(gsub("\\s+", "", v))
distintos <- nombres[!(nombres %in% names(marca_reg)) |
                     norm(valores) != norm(vapply(nombres, function(n) marca_reg[[n]] %||% "", character(1)))]
ok(sprintf("hd_marca.css: its %d values equal the registry's", length(nombres)),
   length(nombres) > 30L && (length(distintos) == 0L || { message("  differ: ", paste(distintos, collapse = ", ")); FALSE }))

# ── 6. Contrast (R/tema_contraste.R, the Diseñador 2's) ─────────────────────
sys.source("R/tema_contraste.R", envir = tema)
contraste <- tema$tema_contraste
.PARES <- list(
  c("texto", "fondo"), c("texto", "superficie"), c("texto", "superficie-2"),
  c("texto-2", "superficie"), c("texto-2", "fondo"),
  c("primario-texto", "primario"), c("primario-texto", "primario-hover"),
  c("texto-inverso", "barra-fondo"), c("barra-texto", "barra-fondo"),
  c("encabezado-texto", "encabezado"), c("enlace", "superficie"),
  c("acento-texto", "superficie"),
  c("exito", "superficie"), c("peligro", "superficie"), c("info", "superficie"),
  c("exito-texto", "exito-suave"), c("peligro-texto", "peligro-suave"),
  c("aviso-texto", "aviso-suave"), c("info", "info-suave"),
  c("exito-texto", "superficie"), c("peligro-texto", "superficie"), c("aviso-texto", "superficie"),
  c("encabezado-texto", "encabezado-hover"), c("encabezado-texto", "pago-encabezado"),
  c("oscuro-texto", "oscuro-fondo"), c("oscuro-texto", "oscuro-fondo-2"),
  c("oscuro-texto-2", "oscuro-fondo"),
  c("foco-color", "superficie"), c("foco-color", "fondo"),
  c("oscuro-exito", "oscuro-fondo"), c("oscuro-peligro", "oscuro-fondo"),
  c("cobro", "superficie"), c("pago", "superficie"), c("provision", "superficie"),
  c("texto", "seleccion"), c("texto", "hover"),
  c("etq-a-texto", "etq-a-pastilla"), c("etq-b-texto", "etq-b-pastilla"),
  c("etq-c-texto", "etq-c-pastilla")
)
# Clásico pairs that fail today (its values are what the app has): they may
# not get worse, and a migration that fixes one removes it from here.
# Text on a soft fill uses exito-texto / peligro-texto, which pass (the fills
# alone, #198754 on #D1E7DD at 3.49, are not text pairs). Can only shrink.
.CLASICO_BAJO_AA <- c("oscuro-peligro/oscuro-fondo")   # 4.15: Bancos' red on its dark band today (TEMA_e4_mapas2 Q3)
ok("every listed Clásico exception is a pair this test checks",
   all(.CLASICO_BAJO_AA %in% vapply(.PARES, paste, character(1), collapse = "/")))
for (t in names(tema$HD_TEMAS)) {
  tk <- tema$tema_tokens(t)
  for (p in .PARES) {
    id <- paste(p, collapse = "/")
    r <- contraste(tk[[p[1]]], tk[[p[2]]])
    if (t == "clasico" && id %in% .CLASICO_BAJO_AA)
      ok(sprintf("[%s] %s = %.2f (below AA today, listed)", t, id, r), r < 4.5)
    else
      ok(sprintf("[%s] %s = %.2f >= 4.5", t, id, r), r >= 4.5)
  }
}

# ── 7. Rule 29 and the Bootstrap theme ──────────────────────────────────────
exprs <- parse("R/tema_registro.R", keep.source = FALSE)
literal <- function(e) {
  if (is.atomic(e) || is.name(e)) return(TRUE)
  if (is.call(e) && identical(e[[1]], as.name("function"))) return(TRUE)
  if (is.call(e) && as.character(e[[1]])[1] %in% c("list", "c")) return(all(vapply(as.list(e)[-1], literal, logical(1))))
  FALSE
}
ok("R/tema_registro.R: only assignments of literal data or functions at the top",
   all(vapply(exprs, function(e) is.call(e) && identical(e[[1]], as.name("<-")) && literal(e[[3]]), logical(1))))
ok("hd_bs_theme() is identical() to the theme app.R wrote three times",
   identical(tema$hd_bs_theme(),
             bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA")))
app <- leer("app.R")
app_codigo <- sub("#.*$", "", app)   # comments may name the functions
ok("app.R: no bs_theme() of its own, hd_bs_theme() three times, tema_head() once",
   !any(grepl("bs_theme\\(", sub("hd_bs_theme\\(", "", app_codigo))) &&
     sum(grepl("hd_bs_theme\\(\\)", app_codigo)) == 3L && sum(grepl("tema_head\\(\\)", app_codigo)) == 1L)
# app.R does not rely on shinyapps' autoload: it source()s each file of R/
# (T15 and local runs source app.R directly). 2026-11-19: the branch passed
# test_autocarga_shiny and still failed to start in T15 ("could not find
# function hd_bs_theme").
fuente <- grep('^\\s*source\\("R/tema_registro\\.R"\\)', app)
primer_uso <- grep("hd_bs_theme\\(\\)|tema_head\\(\\)", sub("#.*$", "", app))
ok("app.R source()s R/tema_registro.R before its first hd_bs_theme() / tema_head()",
   length(fuente) == 1L && length(primer_uso) > 0L && fuente < min(primer_uso))
cab <- htmltools::renderTags(tema$tema_head())$head
ok("tema_head(): the tokens and the attribute go in <head>, with the default theme",
   grepl("hd_temas.css?v=", cab, fixed = TRUE) &&
     grepl(sprintf("setAttribute('data-hd-tema','%s')", tema$HD_TEMA_OMISION), cab, fixed = TRUE))
ok("the generated CSS puts the default theme on :root",
   any(grepl(sprintf('^:root, \\[data-hd-tema="%s"\\] \\{$', tema$HD_TEMA_OMISION), leer("www/hd_temas.css"))))

# ── 8. Clásico's legacy tokens (--hd-leg-<rol>-<hex>) ───────────────────────
# A migration (stage 4) may add legacy tokens, but only by removing at least
# as many hand-written colours: raise .TOPE_LEGADO and lower .TRINQUETE in
# the same commit. Their sum never grows. Then the legacy only shrinks.
.TOPE_LEGADO   <- 59L   # stage 4: cashflow_preview.css (70 -> 0) +22, pasivos_table_styles.R (39 -> 4) +13; TEMA-P1: pasivos' dead CSS gone, -13; treasury_map_module.R (76 -> 4) +37
.TOTAL_INICIAL <- 1338L   # sum(.TRINQUETE) + .TOPE_LEGADO on e2d3882 + TEMA stage 1
leg <- tema$.HD_LEGADO
ok(sprintf("legacy tokens: %d (ratchet %d)", length(leg), .TOPE_LEGADO), length(leg) <= .TOPE_LEGADO)
ok(sprintf("hand-written + legacy never grows: %d <= %d", sum(.TRINQUETE) + .TOPE_LEGADO, .TOTAL_INICIAL),
   sum(.TRINQUETE) + .TOPE_LEGADO <= .TOTAL_INICIAL)
# A legacy token points to a theme role or to a common (etq-*), never to the
# palette. A colour token holds the hex in its name; a shadow token, a shadow
# with that colour and the hash of its value.
legado_valido <- function(leg) vapply(names(leg), function(n) {
  p <- tema$tema_legado_partes(n)
  !is.null(p) && p$rol %in% c(roles[[1]], names(tema$.HD_COMUNES)) &&
    # A tint (p<N>): 1..99, a colour role that is #rrggbb in every theme.
    (is.null(p$pct) || (p$pct >= 1L && p$pct <= 99L && !p$rol %in% tema$HD_ROLES_SOMBRA &&
       all(vapply(names(tema$HD_TEMAS), function(t)
         grepl("^#[0-9A-Fa-f]{6}$", c(tema$.HD_COMUNES, tema$HD_TEMAS[[t]]$tokens)[[p$rol]] %||% ""), logical(1))))) &&
    (if (p$rol %in% tema$HD_ROLES_SOMBRA)
       tema$.tema_sombra_con_color(leg[[n]], p$hex) && identical(p$hash, tema$tema_hash_sombra(leg[[n]]))
     else is.null(p$hash) && identical(toupper(leg[[n]]), p$hex))
}, logical(1))
ok("every legacy token is <rol>-<hex> of a real role or common; a colour holds that hex, a shadow role a shadow with that colour",
   all(legado_valido(leg)))
ok("legacy check: a common with another hex fails, the palette fails, a common with its hex passes",
   !legado_valido(list(`etq-a-texto-c62828` = "#B42318")) &&
     !legado_valido(stats::setNames(list("#123456"), paste0(names(tema$.HD_PALETA)[1], "-123456"))) &&
     legado_valido(list(`etq-a-texto-c62828` = "#C62828")))
ok("www/hd_clasico_legado.css is what scripts/generar_temas_css.R generates",
   identical(sin_fin(leer("www/hd_clasico_legado.css")), sin_fin(tema$tema_legado_css())))
ok("tema_head() links the legacy file too",
   grepl("hd_clasico_legado.css?v=", cab, fixed = TRUE))

# The mechanism, on a registry copy with one legacy token.
prueba <- new.env(); sys.source("R/tema_registro.R", envir = prueba)
s1 <- "0 4px 20px rgba(0, 0, 0, 0.45)"; s2 <- "0 2px 6px rgba(0,0,0,0.06)"
n1 <- paste0("sombra-000000-", tema$tema_hash_sombra(s1)); n2 <- paste0("sombra-000000-", tema$tema_hash_sombra(s2))
prueba$.HD_LEGADO <- stats::setNames(list("#0D1B2A", s1, s2), c("texto-2-0d1b2a", n1, n2))
ok("legacy shadows: two shadows of one colour get two names (hash), spaces and case do not matter",
   n1 != n2 && identical(tema$tema_hash_sombra(s1), tema$tema_hash_sombra("0 4PX 20px  rgba(0,0,0,0.45)")))
ok("legacy shadows: name parts read back (rol, hex, hash); a colour has no hash",
   identical(tema$tema_legado_partes(n1)[c("rol", "hex")], list(rol = "sombra", hex = "#000000")) &&
     identical(tema$tema_legado_partes(n1)$hash, tema$tema_hash_sombra(s1)) &&
     identical(tema$tema_legado_partes("texto-2-0d1b2a"), list(rol = "texto-2", hex = "#0D1B2A", hash = NULL, pct = NULL)))
ok("legacy shadows: the whole shadow is kept and must use the colour in its name",
   prueba$.tema_sombra_con_color(s1, "#000000") && !prueba$.tema_sombra_con_color(s1, "#1E3A5F") &&
     identical(prueba$tema_color(paste0("leg-", n1), "clasico"), s1) &&
     identical(prueba$tema_color(paste0("leg-", n2), "marca"), prueba$tema_color("sombra", "marca")))
ok("tema_mezcla(): rgba like color-mix over transparent; flattened onto a background with `sobre`; leg-* too",
   identical(tema$tema_mezcla("provision", 25, "clasico"), "rgba(124, 58, 237, 0.25)") &&
     identical(tema$tema_mezcla("provision", 100, "clasico", sobre = "#FFFFFF"), "#7C3AED") &&
     identical(tema$tema_mezcla("provision", 0, "clasico", sobre = "#FFFFFF"), "#FFFFFF") &&
     identical(prueba$tema_mezcla("leg-texto-2-0d1b2a", 50, "clasico"), "rgba(13, 27, 42, 0.5)"))
css_leg <- paste(prueba$tema_legado_css(), collapse = "\n")
ok("legacy: Clásico gets today's hex, Marca gets the role (R side)",
   identical(prueba$tema_color("leg-texto-2-0d1b2a", "clasico"), "#0D1B2A") &&
     identical(prueba$tema_color("leg-texto-2-0d1b2a", "marca"), prueba$tema_color("texto-2", "marca")))
ok("legacy: the CSS sends every theme to the role and Clásico (and :root by default) to the hex",
   grepl("--hd-leg-texto-2-0d1b2a: var(--hd-texto-2);", css_leg, fixed = TRUE) &&
     grepl("--hd-leg-texto-2-0d1b2a: #0D1B2A;", css_leg, fixed = TRUE) &&
     grepl('[data-hd-tema="clasico"], :root:not([data-hd-tema]) {', css_leg, fixed = TRUE))
ok("retiring: var(--hd-leg-ROL-hex) -> var(--hd-ROL), \"leg-ROL-hex\" -> \"ROL\", nothing else",
   identical(tema$tema_legado_retirar(c(
       ".x { color: var(--hd-leg-texto-2-0d1b2a); border: 1px solid var(--hd-borde); }",
       "style = \"background:var(--hd-leg-encabezado-1e3a5f)\"",
       "tema_color(\"leg-peligro-texto-b91c1c\", tema)",
       "box-shadow: var(--hd-leg-sombra-000000-3fa2c1); x <- tema_color('leg-foco-4a88c7-0a1b2c')",
       "color: #0d1b2a; /* leg-texto-0d1b2a in a comment, unquoted */",
       ".y { background: var(--hd-leg-provision-p15-e0cffc); } tema_color(\"leg-acento-p8-fff7ed\", t)")),
     c(".x { color: var(--hd-texto-2); border: 1px solid var(--hd-borde); }",
       "style = \"background:var(--hd-encabezado)\"",
       "tema_color(\"peligro-texto\", tema)",
       "box-shadow: var(--hd-sombra); x <- tema_color('foco')",
       "color: #0d1b2a; /* leg-texto-0d1b2a in a comment, unquoted */",
       ".y { background: color-mix(in srgb, var(--hd-provision) 15%, var(--hd-superficie)); } tema_color(\"acento-p8\", t)")))
# Tints (p<N>), on a copy of the registry with three of them.
tinte <- new.env(); sys.source("R/tema_registro.R", envir = tinte)
tinte$.HD_LEGADO <- list(`provision-p15-e0cffc` = "#E0CFFC", `acento-p8-fff7ed` = "#FFF7ED", `texto-2-0d1b2a` = "#0D1B2A")
sup_m <- tinte$tema_color("superficie", "marca")
css_t <- paste(tinte$tema_legado_css(), collapse = "\n")
ok("tint: name parts read back with its pct; a tint with a shadow hash is malformed",
   identical(tinte$tema_legado_partes("provision-p15-e0cffc"), list(rol = "provision", hex = "#E0CFFC", hash = NULL, pct = 15L)) &&
     is.null(tinte$tema_legado_partes("sombra-p10-000000-3fa2c1")) &&
     identical(tinte$tema_legado_partes("texto-2-0d1b2a")$rol, "texto-2"))
ok("tint: Clásico gets today's hex; Marca the role at N % over its surface (R side, flattened)",
   identical(tinte$tema_color("leg-provision-p15-e0cffc", "clasico"), "#E0CFFC") &&
     identical(tinte$tema_color("leg-provision-p15-e0cffc", "marca"),
               tinte$tema_mezcla("provision", 15, "marca", sobre = sup_m)) &&
     identical(tinte$tema_color("leg-acento-p8-fff7ed", "marca"), tinte$tema_mezcla("acento", 8, "marca", sobre = sup_m)) &&
     identical(tinte$tema_color("leg-texto-2-0d1b2a", "marca"), tinte$tema_color("texto-2", "marca")))
ok("tint: the CSS sends other themes to color-mix(role N%, surface) and Clásico to the hex",
   grepl("--hd-leg-provision-p15-e0cffc: color-mix(in srgb, var(--hd-provision) 15%, var(--hd-superficie));", css_t, fixed = TRUE) &&
     grepl("--hd-leg-provision-p15-e0cffc: #E0CFFC;", css_t, fixed = TRUE) &&
     grepl("--hd-leg-texto-2-0d1b2a: var(--hd-texto-2);", css_t, fixed = TRUE))
ok("tint: tema_color(\"<rol>-p<N>\") after retiring = the same colour Marca had; unknown or non-colour roles still fail",
   identical(tinte$tema_color("provision-p15", "marca"), tinte$tema_color("leg-provision-p15-e0cffc", "marca")) &&
     inherits(tryCatch(tinte$tema_color("noexiste-p15", "marca"), error = function(e) e), "error") &&
     inherits(tryCatch(tinte$tema_color("sombra-p15", "marca"), error = function(e) e), "error"))
ok("tint check: pct out of range, a shadow role, a wrong hex all fail; a good one passes",
   legado_valido(list(`provision-p15-e0cffc` = "#E0CFFC")) &&
     !legado_valido(list(`provision-p15-e0cffd` = "#E0CFFC")) &&
     !legado_valido(list(`sombra-p15-000000` = "0 1px 2px rgba(0,0,0,0.1)")) &&
     !legado_valido(list(`provision-p100-e0cffc` = "#E0CFFC")) && !legado_valido(list(`provision-p0-e0cffc` = "#E0CFFC")))
sesion <- function(valor) list(userData = list(hd_tema = function() valor))
ok("tema_de_sesion(): the session's theme; the default with no session, no value or an unknown one",
   identical(tema$tema_de_sesion(NULL), tema$HD_TEMA_OMISION) &&
     identical(tema$tema_de_sesion(sesion("marca")), "marca") &&
     identical(tema$tema_de_sesion(sesion("oscuro")), tema$HD_TEMA_OMISION) &&
     identical(tema$tema_de_sesion(sesion(NULL)), tema$HD_TEMA_OMISION) &&
     identical(tema$tema_de_sesion(list(userData = list())), tema$HD_TEMA_OMISION))
ok("the retiring script exists and uses tema_legado_retirar()",
   file.exists("scripts/retirar_legado_clasico.R") &&
     any(grepl("tema_legado_retirar(", leer("scripts/retirar_legado_clasico.R"), fixed = TRUE)))

# ── 9. The Bootstrap bridge (www/hd_tema_bootstrap.css, TEMA stage 2) ───────
# Clásico must not move: it has no block, so no rule of the bridge can match
# it. Every rule starts with html[data-hd-tema="<a theme with puente>"].
con_puente <- names(tema$HD_TEMAS)[vapply(tema$HD_TEMAS, function(x) isTRUE(x$puente), logical(1))]
ok("every theme says whether it has a bridge; Clásico has none",
   all(vapply(tema$HD_TEMAS, function(x) is.logical(x$puente) && length(x$puente) == 1L && !is.na(x$puente), logical(1))) &&
     identical(tema$HD_TEMAS$clasico$puente, FALSE) && length(con_puente) > 0L)
ok("tema_head() links the bridge, after the tokens it points at",
   grepl("hd_tema_bootstrap.css?v=", cab, fixed = TRUE) &&
     regexpr("hd_temas.css?v=", cab, fixed = TRUE) < regexpr("hd_tema_bootstrap.css?v=", cab, fixed = TRUE))
ok("www/hd_tema_bootstrap.css is what scripts/generar_temas_css.R generates",
   identical(sin_fin(leer("www/hd_tema_bootstrap.css")), sin_fin(tema$tema_puente_css())))
puente <- leer("www/hd_tema_bootstrap.css")
puente_txt <- paste(puente, collapse = "\n")
puente_txt <- gsub("(?s)/\\*.*?\\*/", "", puente_txt, perl = TRUE)   # comments span lines
selectores <- trimws(unlist(strsplit(unlist(regmatches(puente_txt, gregexpr("(?<=^|\\})[^{}]+(?=\\{)", puente_txt, perl = TRUE))), ",")))
selectores <- selectores[nzchar(selectores)]
raices <- sprintf('html[data-hd-tema="%s"]', con_puente)
ok(sprintf("bridge: all %d selectors start with html[data-hd-tema=\"<theme with bridge>\"]", length(selectores)),
   length(selectores) > 50L &&
     all(vapply(selectores, function(s) any(startsWith(s, raices) &
                                              (nchar(s) == nchar(raices) | substr(s, nchar(raices) + 1L, nchar(raices) + 1L) == " ")),
                logical(1))))
ok("bridge: no theme without a bridge is named in it (Clásico included)",
   !any(vapply(setdiff(names(tema$HD_TEMAS), con_puente), function(t)
     grepl(sprintf('data-hd-tema="%s"', t), puente_txt, fixed = TRUE), logical(1))))
plantillas <- unlist(lapply(tema$.tema_puente_bloques(), function(b) unname(b$decl)))
fijas <- gsub("\\{[^}]+\\}", "", plantillas)
ok("bridge: no colour written by hand in a template (only {rol}, {rgb:rol}, {rgba:rol:a})",
   length(plantillas) > 100L && cuenta(fijas, RE_HEX) == 0L && cuenta(fijas, RE_FUN) == 0L)
ok("bridge: every var(--hd-*) of the generated file is a role or a común",
   all(sub("^var\\(--hd-", "", unlist(regmatches(puente, gregexpr("var\\(--hd-[a-zA-Z0-9-]+", puente)))) %in%
         c(names(tema$.HD_COMUNES), roles[[1]])))
ok("bridge: each *-rgb triplet is its role's hex in that theme",
   all(vapply(con_puente, function(t) {
     tk <- tema$tema_tokens(t)
     trip <- function(r) paste(strtoi(substring(tk[[r]], c(2, 4, 6), c(3, 5, 7)), 16L), collapse = ", ")
     blq <- tema$tema_puente_css()
     identical(sum(grepl(paste0("--bs-primary-rgb: ", trip("primario"), ";"), blq, fixed = TRUE)), 1L) &&
       identical(sum(grepl(paste0("--bs-danger-rgb: ", trip("peligro"), ";"), blq, fixed = TRUE)), 1L)
   }, logical(1))))
ok("bridge: a bad template stops the generator (unknown role, non-hex role, bad form)",
   inherits(tryCatch(tema$.tema_puente_valor("{no-existe}", "marca"), error = function(e) e), "error") &&
     inherits(tryCatch(tema$.tema_puente_valor("{rgb:sombra}", "marca"), error = function(e) e), "error") &&
     inherits(tryCatch(tema$.tema_puente_valor("{hsl:primario}", "marca"), error = function(e) e), "error") &&
     identical(tema$.tema_puente_valor("{borde} {rgba:primario:0.5}", "marca"), "var(--hd-borde) rgba(19, 33, 58, 0.5)"))
# A container with data-bs-theme="dark" redefines --bs-* inside it, out of
# the bridge's reach.
bs_dark <- unlist(lapply(c("app.R", list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE)),
                         function(f) if (any(grepl("data-bs-theme", sub("#.*$", "", leer(f)), fixed = TRUE))) f))
ok("no data-bs-theme in app.R or R/ (it would hide the bridge)",
   length(bs_dark) == 0L || { message("  in: ", paste(bs_dark, collapse = ", ")); FALSE })
# Contrast of what the bridge paints, in every theme that has it: a button's
# text on its fill and on its hover fill (Bootstrap's 15 % mix where the
# theme has no hover role), an outline button's colour on the surface, and an
# alert's text on its soft fill.
mezclar <- function(hex, pct, con) {   # color-mix(in srgb, hex pct%, con)
  v <- strtoi(substring(hex, c(2, 4, 6), c(3, 5, 7)), 16L) * pct / 100 + (if (con == "white") 255 else 0) * (100 - pct) / 100
  sprintf("#%02X%02X%02X", as.integer(round(v[1])), as.integer(round(v[2])), as.integer(round(v[3])))
}
b <- tema$.HD_PUENTE_BOTONES
for (t in con_puente) {
  tk <- tema$tema_tokens(t)
  for (i in seq_along(b$clase)) {
    f <- tk[[b$fondo[i]]]; x <- tk[[b$texto[i]]]
    h <- if (is.na(b$hover[i])) mezclar(f, 85, b$mezcla[i]) else tk[[b$hover[i]]]
    a <- if (is.na(b$hover[i])) mezclar(f, 80, b$mezcla[i]) else h
    for (par in list(c("fill", f), c("hover", h), c("active", a))) {
      r <- contraste(x, par[2])
      ok(sprintf("[%s] .btn-%s: %s on %s = %.2f >= 4.5", t, b$clase[i], b$texto[i], par[1], r), r >= 4.5)
    }
    r <- contraste(tk[[b$contorno[i]]], tk[["superficie"]])
    ok(sprintf("[%s] .btn-outline-%s: %s on superficie = %.2f >= 4.5", t, b$clase[i], b$contorno[i], r), r >= 4.5)
  }
  for (p in list(c("primario", "primario-suave"), c("exito-texto", "exito-suave"), c("peligro-texto", "peligro-suave"),
                 c("aviso-texto", "aviso-suave"), c("info", "info-suave"), c("enlace", "fondo"), c("texto-2", "fondo"),
                 c("primario", "superficie"), c("texto", "hover"))) {
    r <- contraste(tk[[p[1]]], tk[[p[2]]])
    ok(sprintf("[%s] bridge %s on %s = %.2f >= 4.5", t, p[1], p[2], r), r >= 4.5)
  }
}

# TEMA 2b · the quiet button (.btn-secondary / .btn-default and their
# outlines): its text on its fill at rest, hover and active, read from the
# bridge's own blocks (change a role there and this follows). A transparent
# fill is checked on the page (fondo) and on a card (superficie).
rol_de <- function(tpl) { m <- regmatches(tpl, regexec("^\\{([a-z0-9-]+)\\}$", tpl))[[1]]; if (length(m)) m[2] else NA_character_ }
tranquilos <- Filter(function(bk) any(grepl("btn-(outline-)?secondary", bk$sel)), tema$.HD_PUENTE_BS)
ok(sprintf("bridge: the quiet button has its two blocks (secondary/default and their outlines): %d", length(tranquilos)),
   length(tranquilos) == 2L)
for (t in con_puente) {
  tk <- tema$tema_tokens(t)
  for (bk in tranquilos) {
    for (estado in c("", "hover-", "active-")) {
      tx <- rol_de(bk$decl[[paste0("--bs-btn-", estado, "color")]])
      bg <- bk$decl[[paste0("--bs-btn-", estado, "bg")]]
      fondos <- if (identical(bg, "transparent")) c("fondo", "superficie") else rol_de(bg)
      for (fd in fondos) {
        r <- contraste(tk[[tx]], tk[[fd]])
        ok(sprintf("[%s] %s %s: %s on %s = %.2f >= 4.5", t, bk$sel[1], if (nzchar(estado)) sub("-$", "", estado) else "rest", tx, fd, r),
           !is.na(tx) && !is.na(fd) && r >= 4.5)
      }
    }
  }
}
# The top bar: its text on its fill, in every theme (Clásico's is flatly's).
for (t in names(tema$HD_TEMAS)) {
  tk <- tema$tema_tokens(t)
  r <- contraste(tk[["barra-texto"]], tk[["barra-fondo"]])
  ok(sprintf("[%s] the top bar: barra-texto on barra-fondo = %.2f >= 4.5", t, r), r >= 4.5)
}
# The fragile point: the bridge beats bslib's .btn-default only by repeating
# its :not() chain, and paints the bar only through --bslib-navbar-light-bg.
# Both are read from the CSS bslib compiles today: if bslib changes either,
# this fails instead of the bridge falling behind in silence.
bs_css <- tryCatch({
  deps <- bslib::bs_theme_dependencies(tema$hd_bs_theme())
  d <- deps[[which(vapply(deps, function(x) x$name, "") == "bootstrap")[1]]]
  paste(readLines(file.path(d$src$file, d$stylesheet)[1], warn = FALSE), collapse = "\n")
}, error = function(e) { message("  ERROR compiling bslib: ", conditionMessage(e)); "" })
ok("bslib still writes .btn-default's selector as .HD_BTN_DEFAULT_SEL (the :not() chain the bridge repeats)",
   grepl(paste0(".btn-secondary,", tema$.HD_BTN_DEFAULT_SEL, "{"), bs_css, fixed = TRUE))
ok("bslib still paints the bar with var(--bslib-navbar-light-bg), and flatly sets it on .navbar",
   grepl(":where(.navbar){background-color:var(--bslib-navbar-light-bg)}", bs_css, fixed = TRUE) &&
     grepl(".navbar{--bslib-navbar-light-bg:", bs_css, fixed = TRUE))
ok("the generated bridge sets the bar's --bslib-navbar-light-bg to barra-fondo under each theme with a bridge",
   all(vapply(con_puente, function(t) grepl(paste0("html\\[data-hd-tema=\"", t, "\"\\] \\.navbar \\{[^}]*--bslib-navbar-light-bg: var\\(--hd-barra-fondo\\);"),
                                              puente_txt), logical(1))))
ok("the generated bridge carries the whole chain under each theme with a bridge",
   all(vapply(con_puente, function(t) grepl(paste0("html[data-hd-tema=\"", t, "\"] ", tema$.HD_BTN_DEFAULT_SEL),
                                              puente_txt, fixed = TRUE), logical(1))))

cat(sprintf("\n=== %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
