# =============================================================================
# R/avisos_correos.R
# The two weekly emails of Panorama (frente D): Monday "la semana que
# empieza" (8:00) and Friday "cómo cerró la semana" (17:00). First design
# approved by Luis on 2026-11-11 (requisiciones/D_mockup_correos.html);
# redone in T45 (reportes/T45_paso0.md, the Coordinador's single proposal):
# cards that explain themselves, every figure with its code, top 10, a day
# by day and a context chart, and a Friday with amounts against the plan.
#
#   correo_destinatarios()  who gets them: live client users with a valid
#                           email who hold can_receive_weekly_email (dev and
#                           admin by default, others per user) and did not
#                           turn them off (preference correos_semana = FALSE)
#   correo_lunes_html()     the Monday email, from correo_datos_lunes()
#   correo_viernes_html()   the Friday email, from correo_datos_viernes()
#
# Email HTML: inline styles and tables only (Gmail and Outlook drop <style>,
# flex and SVG); bars are table cells. Fraunces shows where the client loads
# web fonts (Apple Mail, iOS); elsewhere Georgia. Every visible text comes
# from the dictionary (avisos.correo.*, and semana.tarjeta.* shared with
# Panorama) in the recipient's language; dynamic text is HTML-escaped. A
# text marked *like this* in the dictionary is set in italic.
#
# Never prints: the job runs in a public repo's logs (see avisos_job.R).
# =============================================================================

# The mails' colours come from the theme registry (R/tema_registro.R), in the
# theme the client CHOSE (TEMA stage 5a); a client that chose none gets Marca,
# as every client did before (Luis approved the weekly mail in Marca). Each
# field is a role per theme. Marca's column is the 4b one, untouched; Clásico's
# reads each side as its own family -- collections the gold (acento), payments
# the navy (primario), lighter where the mail is lighter ("<rol>-pN": the role at
# N % over the surface) -- never a warning colour (the Diseñador, 2026-11-22).
# A theme with no column of its own takes Marca's.
CORREO_TEMA <- "marca"
.CORREO_ROLES <- list(
  marca = c(
    fondo = "lino", papel = "superficie", tinta = "texto", gris = "texto-2",
    marino = "primario", dorado = "acento", dorado_osc = "acento-texto", linea = "lino-bd",   # dorado_osc is text (the bullets): acento-texto (the Coordinador, 2026-11-24)
    linea_tabla = "superficie-2", pie = "fondo", arena = "arena", arena_tinta = "arena-fg",
    gris_claro = "niebla-osc", cobre = "aviso-texto", revision = "aviso-suave", revision_tinta = "ambar-osc",
    verde = "exito", logro_tinta = "acento-texto", riel = "lino-riel", reprog = "texto-tenue",   # TEMA 6-9: readable (the Coordinador)
    por_vencer = "bruma", cambio = "borde", liquidado_ar = "arena-osc", liquidado_ap = "pizarra-clara",
    linea_oscura = "marino-claro"),
  clasico = c(
    fondo = "superficie-2", papel = "superficie", tinta = "texto", gris = "texto-2",
    marino = "primario", dorado = "acento", dorado_osc = "acento-texto", linea = "borde",
    linea_tabla = "superficie-2", pie = "fondo", arena = "acento-p12", arena_tinta = "acento-texto",
    gris_claro = "primario-suave", cobre = "aviso-texto", revision = "aviso-suave", revision_tinta = "aviso-texto",
    verde = "exito", logro_tinta = "acento-texto", riel = "superficie-2", reprog = "texto-2",   # TEMA 6-9: readable (Clásico's texto-tenue is 2.5)
    por_vencer = "borde-fuerte", cambio = "borde", liquidado_ar = "acento-p45", liquidado_ap = "primario-p40",
    linea_oscura = "primario-hover"))
.correo_colores <- function(tema = CORREO_TEMA) {
  # A theme the registry does not know is Marca, whole: tema_color() would
  # read it as the default theme and mix it with Marca's column (the Diseñador).
  if (!isTRUE(tema_valido(tema))) tema <- CORREO_TEMA
  tema <- tolower(trimws(tema))
  roles <- .CORREO_ROLES[[tema]] %||% .CORREO_ROLES[[CORREO_TEMA]]
  as.list(vapply(roles, tema_color, character(1), tema = tema))
}
# The weekly mail's theme for a client: correo_tema(cid), in R/tema_registro.R
# since TEMA 6-9 (one rule for every mail): the one it chose, else Marca; read
# once per client per run (5-minute cache), a failed read is Marca. Always the
# cid the job passes, never CLIENT_ID from the environment.
# Lazy: shiny sources R/ in alphabetical order, so tema_color() (R/tema_registro.R)
# does not exist yet when this file loads; the list is built on first use.
delayedAssign("CORREO_COLORES", .correo_colores())
.CORREO_EMAIL_RE <- "^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$"

# ── Destinatarios ────────────────────────────────────────────────────────────

# `usuarios`: auth_load_usuarios(cid). `config`: the client's tiers_config.rds
# (or NULL). `prefs`: preferencias.rds as a data frame (usuario, datos).
# Returns one row per recipient: usuario, email, tier, idioma, ver_cxc, and
# permisos (a list column: the resolved permissions, for avisos_visibles()).
# Pure: the job reads the three files (strictly) and passes them in.
correo_destinatarios <- function(usuarios, config = NULL, prefs = NULL) {
  vacio <- tibble::tibble(usuario = character(), email = character(), tier = character(),
                          idioma = character(), ver_cxc = logical(), permisos = list())
  if (!is.data.frame(usuarios) || !nrow(usuarios)) return(vacio)
  col <- function(n, def) if (n %in% names(usuarios)) usuarios[[n]] else rep(def, nrow(usuarios))
  vivo  <- !(col("deleted", FALSE) %in% TRUE) & !(col("activo", TRUE) %in% FALSE)
  tier  <- as.character(col("tier", NA_character_))
  email <- tolower(trimws(as.character(col("email", NA_character_))))
  user  <- tolower(trimws(as.character(col("username", NA_character_))))
  perm  <- as.character(col("permisos", "{}"))
  filas <- lapply(which(vivo & tier %in% niveles_cliente() & !is.na(email) &
                          grepl(.CORREO_EMAIL_RE, email) & !is.na(user) & nzchar(user)), function(i) {
    u <- list(user = user[i], tier = tier[i],
              permisos = permisos_resueltos(tier[i], if (is.na(perm[i])) "{}" else perm[i], config))
    if (!puede(u, "can_receive_weekly_email")) return(NULL)
    # T69b: the weekly email is treasury (payments, collections, the Agenda):
    # a tier with neither the Agenda nor the Calendario (Compras: PAN + REQ)
    # never gets it, whatever its permissions say.
    if (!nivel_pestana_permitida(tier[i], "PH") && !nivel_pestana_permitida(tier[i], "CAL")) return(NULL)
    p <- .correo_prefs(prefs, user[i])
    if (identical(p$correos_semana, FALSE)) return(NULL)
    idioma <- p$idioma %||% "es"
    # Values first: inside tibble() a name refers to the column just made.
    t_i <- tier[i]; cxc <- is.null(nivel_calendario(t_i))
    tibble::tibble(usuario = user[i], email = email[i], tier = t_i,
                   idioma = if (idioma %in% i18n_idiomas()) idioma else "es",
                   ver_cxc = cxc, permisos = list(u$permisos))
  })
  dplyr::bind_rows(vacio, filas)
}

.correo_prefs <- function(prefs, usuario) {
  if (!is.data.frame(prefs) || !nrow(prefs)) return(list())
  i <- which(tolower(prefs$usuario) == usuario)
  if (!length(i)) return(list())
  tryCatch(as.list(jsonlite::fromJSON(prefs$datos[i[1]], simplifyVector = TRUE)), error = function(e) list())
}

# ── Formato ──────────────────────────────────────────────────────────────────
# Amounts with monto_moneda() and SAP's date with sap_al_texto() (R/
# ui_components.R, T46): the same words as Panorama, no local copies. Every
# figure carries its code, the base too (Luis, T45).

.cr_e <- function(x) htmltools::htmlEscape(as.character(x))
# Dictionary text, escaped, with *this* in italic. Placeholders are filled
# before escaping, so a value can never inject markup.
.cr_t <- function(clave, idioma, ...) {
  txt <- .cr_e(tr(clave, idioma = idioma, ...))
  gsub("\\*([^*]+)\\*", "<i>\\1</i>", txt)
}
.cr_tn <- function(clave, n, idioma) .cr_e(tr_n(clave, n, idioma = idioma))

# One amount, escaped and never split across lines; "+" before a positive
# one when `signo`, "≈ " before an approximate one (banxico-fix).
.cr_m <- function(x, moneda, signo = FALSE, aprox = FALSE)
  paste0('<span style="white-space:nowrap">', if (isTRUE(aprox)) "&#8776; " else "",
         if (signo && isTRUE(as.numeric(x) > 0)) "+" else "", .cr_e(monto_moneda(x, moneda)), '</span>')
.cr_pts <- function(x) sprintf("%.1f", abs(x))
.cr_pct <- function(x) sprintf("%.0f%%", x)

correo_fecha <- function(fecha, idioma, forma = "larga") {
  d <- as.Date(fecha)
  tr(paste0("avisos.correo.fecha_", forma), idioma = idioma,
     dia = tr(paste0("avisos.correo.dia.", format(d, "%u")), idioma = idioma),
     dia_corto = tr(paste0("avisos.correo.dia_corto.", format(d, "%u")), idioma = idioma),
     n = as.integer(format(d, "%d")),
     mes = tr(paste0("avisos.correo.mes.", as.integer(format(d, "%m"))), idioma = idioma))
}

correo_rango <- function(desde, hasta, idioma) {
  a <- as.Date(desde); b <- as.Date(hasta)
  mes <- function(d) tr(paste0("avisos.correo.mes.", as.integer(format(d, "%m"))), idioma = idioma)
  if (format(a, "%m") == format(b, "%m"))
    tr("avisos.correo.rango_mes", idioma = idioma, d1 = as.integer(format(a, "%d")),
       d2 = as.integer(format(b, "%d")), mes = mes(a))
  else
    tr("avisos.correo.rango", idioma = idioma, d1 = as.integer(format(a, "%d")), mes1 = mes(a),
       d2 = as.integer(format(b, "%d")), mes2 = mes(b))
}

# "del lun 16 al vie 20": the range a card speaks of.
.cr_del_al <- function(desde, hasta, idioma)
  tr("avisos.correo.del_al", idioma = idioma, a = correo_fecha(desde, idioma, "corta"),
     b = correo_fecha(hasta, idioma, "corta"))

.cr_mayus <- function(s) paste0(toupper(substr(s, 1, 1)), substring(s, 2))
.cr_flecha <- function(delta) if (delta > 0) "&#9650;" else if (delta < 0) "&#9660;" else "&#8226;"

# The currencies of `montos`, the base first.
.cr_orden <- function(montos, base) {
  n <- names(montos)
  c(intersect(base, n), setdiff(n, base))
}
.cr_valor <- function(montos, m) if (m %in% names(montos)) as.numeric(montos[[m]]) else 0

# The "Para revisión" lines: each umbral alert, in the recipient's language.
correo_revision <- function(alertas, idioma = "es") {
  if (is.null(alertas) || !nrow(alertas)) return(character())
  vapply(seq_len(nrow(alertas)), function(i)
    tr(paste0("avisos.umbral.cxc_", alertas$direccion[i], ".titulo"), idioma = idioma,
       empresa = alertas$empresa[i], moneda = alertas$moneda[i], pts = sprintf("%.1f", abs(alertas$delta[i]))),
    character(1))
}

# ── Piezas del correo (tablas, estilos en línea) ─────────────────────────────
# Email HTML: tables and inline styles only. The page is fluid up to 568 px
# (a fixed width="568" never shrank on a phone, T45 H5); Outlook gets its
# fixed table through an <!--[if mso]> wrapper. Two cards side by side are
# inline-blocks that stack on a phone without media queries.

delayedAssign(".cr_C", CORREO_COLORES)
# One mail's palette: .cr_C (what every helper below reads) is the mail's theme
# while it is built, and back to the default after (also on error).
.correo_con_tema <- function(tema, construir) {
  env <- environment(sys.function())
  previo <- get(".cr_C", envir = env)
  assign(".cr_C", .correo_colores(tema %||% CORREO_TEMA), envir = env)
  on.exit(assign(".cr_C", previo, envir = env), add = TRUE)
  construir()
}
.cr_SERIF <- "Fraunces,Georgia,serif"
.cr_SANS  <- "Lato,Arial,sans-serif"

.cr_tabla <- function(..., estilo = "") paste0(
  '<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0" style="', estilo, '">',
  ..., '</table>')
.cr_esp <- function(h) paste0('<div style="height:', h, 'px;line-height:', h, 'px;font-size:1px">&nbsp;</div>')
.cr_fila <- function(x, pad = "0 28px 12px") paste0('<tr><td style="padding:', pad, '">', x, '</td></tr>')
.cr_seccion <- function(titulo, sub = NULL, pad = "24px 28px 10px") .cr_fila(paste0(
  '<div style="font-family:', .cr_SERIF, ';font-weight:500;font-size:20px;color:', .cr_C$marino, '">', titulo, '</div>',
  if (!is.null(sub)) paste0('<div style="font-size:13px;line-height:1.45;color:', .cr_C$gris, ';padding-top:3px">', sub, '</div>')),
  pad)
.cr_nota <- function(txt, pad = "2px 28px 4px") .cr_fila(paste0(
  '<div style="font-size:12px;line-height:1.5;color:', .cr_C$gris, '">', txt, '</div>'), pad)

.cr_encabezado <- function(fecha_txt, eyebrow, meta, titulo, linea, idioma) paste0(
  '<tr><td style="background:', .cr_C$marino, ';padding:26px 28px 24px">',
  .cr_tabla('<tr><td style="font-weight:900;font-size:12px;letter-spacing:.26em;color:', .cr_C$dorado, '">&#9672;&nbsp; ',
            .cr_t("avisos.correo.marca", idioma), '</td><td align="right" style="font-size:12px;color:', .cr_C$gris_claro, '">',
            fecha_txt, '</td></tr>'),
  .cr_esp(14),
  '<div style="font-size:11px;font-weight:900;letter-spacing:.22em;color:', .cr_C$dorado, '">', eyebrow, '</div>',
  .cr_esp(4), '<div style="font-size:13px;color:', .cr_C$gris_claro, '">', meta, '</div>',
  .cr_esp(12),
  '<div style="font-family:', .cr_SERIF, ';font-weight:500;font-size:27px;line-height:1.2;color:', .cr_C$papel, '">', titulo, '</div>',
  if (nzchar(linea)) paste0(.cr_esp(10), '<div style="font-size:14px;line-height:1.5;color:', .cr_C$gris_claro, '">', linea, '</div>'),
  '</td></tr>')

# One currency per line, never split: the base first and large. `colorear`:
# green when >= 0, copper below. `aprox`: the currencies holding an
# approximate amount.
.cr_renglones <- function(montos, base, color, grande = 24, signo = FALSE, colorear = FALSE, aprox = character()) {
  if (!length(montos)) montos <- stats::setNames(0, base)
  paste(vapply(seq_along(.cr_orden(montos, base)), function(i) {
    m <- .cr_orden(montos, base)[i]; v <- .cr_valor(montos, m)
    col <- if (colorear) (if (v >= 0) .cr_C$verde else .cr_C$cobre) else color
    paste0('<div style="font-family:', .cr_SERIF, ';font-size:', if (i == 1) grande else 17, 'px;line-height:1.3;color:', col, '">',
           .cr_m(v, m, signo, m %in% aprox), '</div>')
  }, character(1)), collapse = "")
}

# A card: LABEL, figures, one line that says what it means (the language
# Panorama uses too, semana.tarjeta.*). `extra` goes between figures and line.
.cr_tarjeta <- function(etiqueta, cuerpo, explica, estilo = c("borde", "arena", "marino"), extra = "") {
  estilo <- match.arg(estilo)
  caja <- switch(estilo, arena = paste0("background:", .cr_C$arena), marino = paste0("background:", .cr_C$marino),
                 borde = paste0("border:1px solid ", .cr_C$linea))
  c_eti <- switch(estilo, arena = .cr_C$arena_tinta, marino = .cr_C$dorado, borde = .cr_C$gris)
  c_exp <- switch(estilo, arena = .cr_C$arena_tinta, marino = .cr_C$gris_claro, borde = .cr_C$gris)
  .cr_tabla(paste0('<tr><td style="', caja, ';border-radius:14px;padding:16px 18px">',
                   '<div style="font-size:11px;font-weight:900;letter-spacing:.12em;text-transform:uppercase;color:', c_eti, '">',
                   etiqueta, '</div>', .cr_esp(6), cuerpo, extra, .cr_esp(8),
                   '<div style="font-size:13px;line-height:1.45;color:', c_exp, '">', explica, '</div></td></tr>'))
}

# Two cards side by side; on a phone they stack. One card: full width.
.cr_dos <- function(a, b = NULL) {
  if (is.null(b)) return(.cr_fila(a))
  celda <- function(x) paste0('<div style="display:inline-block;vertical-align:top;width:100%;max-width:248px;padding:0 0 12px 0">',
                              x, '</div>')
  paste0('<tr><td style="padding:0 22px 0 28px;font-size:0">',
         '<!--[if mso]><table role="presentation" width="100%"><tr><td width="50%" valign="top"><![endif]-->', celda(a),
         '<!--[if mso]></td><td width="50%" valign="top"><![endif]--><div style="display:inline-block;width:12px"></div>',
         celda(b), '<!--[if mso]></td></tr></table><![endif]--></td></tr>')
}

# A proportional bar: a table whose first cell takes `pct` of the width.
.cr_barra <- function(pct, color, alto = 8, claro = FALSE, riel = TRUE) {
  w <- max(1, min(100, round(as.numeric(pct))))
  if (is.na(w)) w <- 1
  celda <- function(fondo, ancho) paste0('<td', if (!is.null(ancho)) paste0(' width="', ancho, '%"'), ' style="background:', fondo, ';',
                                         if (claro && identical(fondo, color)) "opacity:.45;" else "",
                                         'height:', alto, 'px;line-height:', alto, 'px;font-size:1px">&nbsp;</td>')
  .cr_tabla(paste0('<tr>', celda(color, w), if (w < 100) celda(if (riel) .cr_C$riel else "transparent", NULL), '</tr>'),
            estilo = "border-radius:3px;overflow:hidden")
}

# A list in two lines per row: counterpart and amount, then company and
# date (and COBRO/PAGO when `con_tipo`). df: .avisos_top().
.cr_lista <- function(df, idioma, con_tipo = FALSE) {
  if (!is.data.frame(df) || !nrow(df)) return("")
  filas <- vapply(seq_len(nrow(df)), function(i) {
    borde <- if (i > 1) paste0("border-top:1px solid ", .cr_C$linea_tabla, ";") else ""
    chip <- if (con_tipo) {
      ar <- identical(df$tipo[i], "AR")
      paste0('<span style="font-size:10px;font-weight:900;letter-spacing:.1em;color:', if (ar) .cr_C$arena_tinta else .cr_C$marino,
             '">', .cr_t(if (ar) "avisos.correo.tipo.cobro" else "avisos.correo.tipo.pago", idioma), '</span> &middot; ')
    } else ""
    paste0('<tr><td width="22" valign="top" style="', borde, 'padding:10px 0;font-size:12px;font-weight:900;color:', .cr_C$gris_claro, '">', i, '</td>',
           '<td style="', borde, 'padding:10px 0">',
           .cr_tabla(paste0('<tr><td style="font-size:14px;font-weight:700;color:', .cr_C$tinta, '">', .cr_e(df$contraparte[i]), '</td>',
                            '<td align="right" valign="top" style="font-size:14px;font-weight:700;color:', .cr_C$marino, ';padding-left:10px">',
                            .cr_m(df$monto[i], df$moneda[i], aprox = isTRUE(df$aprox[i])), '</td></tr>',
                            '<tr><td colspan="2" style="font-size:12px;color:', .cr_C$gris, ';padding-top:2px">', chip,
                            .cr_e(df$empresa[i]), ' &middot; ', .cr_e(correo_fecha(df$fecha[i], idioma, "corta")), '</td></tr>')),
           '</td></tr>')
  }, character(1))
  .cr_fila(.cr_tabla(paste(filas, collapse = "")), "0 28px 4px")
}

# "Ordered by the equivalent in MXN at today's rate; 3 in USD without a
# rate are left out" -- under a list whose currencies are mixed.
.cr_nota_orden <- function(listas, sin_tipo, base, idioma) {
  monedas <- unique(unlist(lapply(listas, function(d) if (is.data.frame(d)) d$moneda)))
  partes <- character()
  if (length(setdiff(monedas, base))) partes <- .cr_t("avisos.correo.nota_orden", idioma, base = base)
  if (length(sin_tipo)) partes <- c(partes, vapply(names(sin_tipo), function(m)
    .cr_t("avisos.correo.nota_sin_tipo", idioma, n = sin_tipo[[m]], moneda = m), character(1)))
  if (!length(partes)) return("")
  .cr_nota(paste(partes, collapse = " "), "6px 28px 0")
}

.cr_vinetas <- function(lineas, simbolo = "&#9670;", tam = "10px") {
  paste(vapply(seq_along(lineas), function(i) paste0(
    '<tr><td width="18" valign="top" style="color:', .cr_C$dorado_osc, ';font-size:', tam, ';padding-top:4px">', simbolo, '</td>',
    '<td style="padding-bottom:', if (i < length(lineas)) 8 else 0, 'px">', lineas[[i]], '</td></tr>'),
    character(1)), collapse = "")
}

.cr_revision <- function(lineas, idioma) {
  if (!length(lineas)) return("")
  .cr_fila(.cr_tabla(paste0(
    '<tr><td style="padding:14px 16px;border-radius:14px;background:', .cr_C$revision, ';color:', .cr_C$revision_tinta,
    ';font-size:14px;line-height:1.5"><b>', .cr_t("avisos.correo.lunes.para_revision", idioma), '</b> ',
    paste(.cr_e(lineas), collapse = "<br>"), '</td></tr>')), "0 28px 4px")
}

.cr_boton_y_pie <- function(url, datos_sap, idioma) paste0(
  '<tr><td align="center" style="padding:24px 28px 28px">',
  '<a href="', .cr_e(url), '" style="display:inline-block;padding:15px 32px;border-radius:999px;background:', .cr_C$marino,
  ';color:', .cr_C$papel, ';font-weight:900;font-size:15px;text-decoration:none">', .cr_t("avisos.correo.boton", idioma),
  ' <span style="color:', .cr_C$dorado, '">&rarr;</span></a></td></tr>',
  '<tr><td style="background:', .cr_C$pie, ';border-top:1px solid ', .cr_C$linea, ';padding:16px 28px;font-size:12px;line-height:1.5;color:', .cr_C$gris, '">',
  .cr_e(.cr_sap(datos_sap, idioma)), '<br>', .cr_t("avisos.correo.pie.monedas", idioma), ' ',
  .cr_t("avisos.correo.pie.apagar", idioma), '</td></tr>')

.cr_sap <- function(datos_sap, idioma) sap_al_texto(datos_sap, tz = UMBRAL_TZ, idioma = idioma)

# Light only (T45d, point 4: on a phone the mail app's dark mode lightened
# the navy and dropped some backgrounds). The meta tags and color-scheme ask
# for light; every background of a <table>/<td> also goes as a bgcolor
# attribute, which the apps that strip CSS still honor. No <style> (Gmail
# cuts it). Gmail on Android in dark mode may still invert: there is no full
# control from the email.
.cr_con_bgcolor <- function(html)
  gsub('<(td|table)((?:(?!bgcolor=)[^>])*?) style="([^"]*?)background:(#[0-9A-Fa-f]{6})',
       '<\\1\\2 bgcolor="\\4" style="\\3background:\\4', html, perl = TRUE)

.cr_documento <- function(asunto, cuerpo) .cr_con_bgcolor(paste0(
  '<!DOCTYPE html><html style="color-scheme:light;supported-color-schemes:light"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  '<meta name="color-scheme" content="light">',
  '<meta name="supported-color-schemes" content="light">',
  '<title>', .cr_e(asunto), '</title>',
  '<link href="https://fonts.googleapis.com/css2?family=Fraunces:ital,wght@0,400;0,500;1,400&amp;family=Lato:wght@400;700;900&amp;display=swap" rel="stylesheet">',
  '</head><body bgcolor="', .cr_C$fondo, '" style="margin:0;padding:0;color-scheme:light;background:', .cr_C$fondo, '">',
  '<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0" style="background:', .cr_C$fondo,
  ';font-family:', .cr_SANS, ';color:', .cr_C$tinta, '"><tr><td align="center" style="padding:24px 10px">',
  '<!--[if mso]><table role="presentation" width="568" cellpadding="0" cellspacing="0" border="0"><tr><td><![endif]-->',
  '<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0" style="width:100%;max-width:568px;background:',
  .cr_C$papel, ';border-radius:18px;overflow:hidden">', cuerpo,
  '</table><!--[if mso]></td></tr></table><![endif]--></td></tr></table></body></html>'))

# ── Gráficas de barras (tablas, nunca SVG) ───────────────────────────────────

# Day by day, base currency: cobros over pagos, each with its figure.
.cr_por_dia <- function(dias, base, idioma, cxc) {
  if (!is.data.frame(dias) || !nrow(dias)) return("")
  mx <- max(c(if (cxc) dias$cobros, dias$pagos, 0))
  if (mx <= 0) return("")
  par <- function(v, color) .cr_tabla(paste0(
    '<tr><td style="padding:2px 0">', .cr_barra(100 * v / mx, color, 9, riel = FALSE), '</td>',
    '<td width="124" align="right" style="font-size:12px;color:', .cr_C$tinta, ';padding-left:8px">', .cr_m(v, base), '</td></tr>'))
  filas <- vapply(seq_len(nrow(dias)), function(i) {
    borde <- if (i > 1) paste0("border-top:1px solid ", .cr_C$linea_tabla, ";") else ""
    paste0('<tr><td width="54" valign="middle" style="', borde, 'padding:8px 0;font-size:12px;font-weight:700">',
           .cr_e(correo_fecha(dias$fecha[i], idioma, "corta")), '</td><td style="', borde, 'padding:8px 0">',
           if (cxc) par(dias$cobros[i], .cr_C$dorado), par(dias$pagos[i], .cr_C$marino), '</td></tr>')
  }, character(1))
  paste0(.cr_fila(.cr_tabla(paste(filas, collapse = "")), "0 28px 4px"),
         .cr_nota(.cr_leyenda(base, idioma, cxc, "avisos.correo.lunes.por_dia_nota")))
}

.cr_leyenda <- function(base, idioma, cxc, clave) paste0(
  if (cxc) paste0('<span style="color:', .cr_C$dorado, '">&#9632;</span> ', .cr_t("avisos.correo.leyenda.cobrar", idioma), ' &nbsp; '),
  '<span style="color:', .cr_C$marino, '">&#9632;</span> ', .cr_t("avisos.correo.leyenda.pagar", idioma), ' &nbsp;&middot;&nbsp; ',
  .cr_t(clave, idioma, base = base))

# Context: real weeks, this one, scheduled ones -- small columns, base only.
.cr_contexto <- function(ctx, base, idioma, cxc, cobertura = NA_real_, faltan = as.Date(character()), k = 4L) {
  if (!is.data.frame(ctx) || !nrow(ctx)) return("")
  alto <- 70
  mx <- max(c(if (cxc) ctx$cobros, ctx$pagos, 0))
  if (mx <= 0) return("")
  col <- function(v, color, claro) {
    h <- max(2, round(alto * v / mx))
    paste0('<td valign="bottom" width="8" style="padding:0 1px">',
           '<table role="presentation" width="8" cellpadding="0" cellspacing="0" border="0"><tr>',
           '<td height="', h, '" style="width:8px;height:', h, 'px;line-height:', h, 'px;font-size:1px;background:', color, ';',
           if (claro) "opacity:.45;" else "", '">&nbsp;</td></tr></table></td>')
  }
  fondo <- function(i) if (ctx$clase[i] == "esta") paste0("background:", .cr_C$pie, ";") else ""
  idx <- seq_len(nrow(ctx))
  celdas <- paste(vapply(idx, function(i) paste0(
    '<td align="center" valign="bottom" height="', alto, '" style="', fondo(i), 'padding:4px 1px 0">',
    '<table role="presentation" cellpadding="0" cellspacing="0" border="0" align="center"><tr>',
    if (cxc) col(ctx$cobros[i], .cr_C$dorado, ctx$clase[i] == "programado"),
    col(ctx$pagos[i], .cr_C$marino, ctx$clase[i] == "programado"), '</tr></table></td>'), character(1)), collapse = "")
  mes <- function(d) tr(paste0("comun.mes.", as.integer(format(d, "%m"))), idioma = idioma)
  etis <- paste(vapply(idx, function(i) paste0(
    '<td align="center" style="', fondo(i), 'padding:5px 0 4px;font-size:10px;color:', .cr_C$gris, ';white-space:nowrap;',
    if (ctx$clase[i] == "esta") "font-weight:900;" else "", '">',
    .cr_e(paste(as.integer(format(ctx$lunes[i], "%d")), mes(ctx$lunes[i]))), '</td>'), character(1)), collapse = "")
  netos <- if (cxc) paste0('<tr>', paste(vapply(idx, function(i) {
    arriba <- ctx$cobros[i] >= ctx$pagos[i]
    paste0('<td align="center" style="', fondo(i), 'padding:0 0 6px;font-size:11px;color:',
           if (arriba) .cr_C$verde else .cr_C$cobre, '">', if (arriba) "&#9650;" else "&#9660;", '</td>')
  }, character(1)), collapse = ""), '</tr>') else ""
  n_real <- sum(ctx$clase == "real"); n_prog <- sum(ctx$clase == "programado")
  cab <- paste0('<tr>',
    if (n_real) paste0('<td colspan="', n_real, '" style="font-size:10px;letter-spacing:.12em;color:', .cr_C$gris,
                       ';padding-bottom:4px;text-transform:uppercase">', .cr_t("avisos.correo.contexto.real", idioma), '</td>'),
    '<td></td>',
    if (n_prog) paste0('<td colspan="', n_prog, '" align="right" style="font-size:10px;letter-spacing:.12em;color:', .cr_C$gris,
                       ';padding-bottom:4px;text-transform:uppercase">', .cr_t("avisos.correo.contexto.programado", idioma), '</td>'),
    '</tr>')
  nota <- paste0(
    if (cxc) paste0('<span style="color:', .cr_C$verde, '">&#9650;</span> ', .cr_t("avisos.correo.contexto.sube", idioma), ' ',
                    '<span style="color:', .cr_C$cobre, '">&#9660;</span> ', .cr_t("avisos.correo.contexto.baja", idioma), ' '),
    .cr_t("avisos.correo.contexto.escala", idioma, base = base), ' ', .cr_m(mx, base), '. ',
    if (n_real) .cr_t("avisos.correo.contexto.real_nota", idioma)
    else if (length(faltan) && length(faltan) < k) .cr_t("avisos.correo.contexto.sin_banco_semanas", idioma,
      semanas = paste(vapply(sort(faltan), function(l) paste(as.integer(format(l, "%d")),
        tr(paste0("comun.mes.", as.integer(format(l, "%m"))), idioma = idioma)), character(1)), collapse = ", "))
    else if (is.na(cobertura)) .cr_t("avisos.correo.contexto.sin_banco", idioma)
    else .cr_t("avisos.correo.contexto.sin_cobertura", idioma, pct = sprintf("%.0f%%", 100 * cobertura)))
  paste0(.cr_fila(.cr_tabla(paste0(cab, '<tr>', celdas, '</tr><tr>', etis, '</tr>', netos),
                            estilo = paste0("border-bottom:1px solid ", .cr_C$linea)), "0 28px 4px"),
         .cr_nota(nota))
}

# ── Lunes · la semana que empieza ────────────────────────────────────────────
# `d`: correo_datos_lunes() (R/avisos_datos.R) plus, per recipient, tambien:
#   lunes, moneda_base, cobros, pagos (named by moneda), cobros_n, pagos_n,
#   cobros_aprox, pagos_aprox, pasivos_n, pasivos, cobros_atrasados,
#   vencidos_pct, vencidos_delta, vencido (by moneda), por_dia, contexto,
#   revision | alertas, top_cobros, top_pagos, sin_tipo, tambien, datos_sap, url
# `dest`: one row of correo_destinatarios() (idioma, ver_cxc). Without CxC
# (Compras) there are no collections anywhere: no cards, bars or list.
correo_lunes_asunto <- function(d, idioma)
  tr("avisos.correo.lunes.asunto", idioma = idioma,
     rango = correo_rango(d$lunes, as.Date(d$lunes) + 4L, idioma))

# The other currencies' net, for the headline's second line.
.cr_neto <- function(cobros, pagos, base) {
  m <- unique(c(.cr_orden(cobros, base), .cr_orden(pagos, base)))
  m <- c(intersect(base, m), setdiff(m, base))
  stats::setNames(vapply(m, function(x) .cr_valor(cobros, x) - .cr_valor(pagos, x), numeric(1)), m)
}

# d$tema: the client's theme (the job puts correo_tema(cid) there); none = Marca.
correo_lunes_html <- function(d, dest) .correo_con_tema(d$tema, function() .correo_lunes_cuerpo(d, dest))
.correo_lunes_cuerpo <- function(d, dest) {
  idioma <- dest$idioma %||% "es"; cxc <- isTRUE(dest$ver_cxc)
  base <- d$moneda_base %||% MONEDA_BASE_OMISION
  lunes <- as.Date(d$lunes); viernes <- lunes + 4L
  rango <- .cr_del_al(lunes, viernes, idioma)
  cobros <- d$cobros %||% numeric(); pagos <- d$pagos %||% numeric()
  neto <- .cr_neto(cobros, pagos, base)
  # The headline compares the base currency only: amounts in different
  # currencies are never added up (integrador, 2026-11-12).
  nb <- neto[[base]]
  titulo <- if (!cxc) .cr_t("avisos.correo.lunes.titulo.sin_cxc", idioma, monto = monto_moneda(round(.cr_valor(pagos, base)), base))
            else if (nb >= 0) .cr_t("avisos.correo.lunes.titulo.cobros", idioma, monto = paste0(if (nb > 0) "+", monto_moneda(round(nb), base)))
            else .cr_t("avisos.correo.lunes.titulo.pagos", idioma, monto = monto_moneda(round(nb), base))
  otras <- setdiff(names(neto), base)
  linea <- if (cxc) paste0(
    .cr_t("avisos.correo.lunes.linea", idioma, base = base, cobrar = monto_moneda(round(.cr_valor(cobros, base)), base),
          pagar = monto_moneda(round(.cr_valor(pagos, base)), base)),
    if (length(otras)) paste0(" ", .cr_t("avisos.correo.lunes.linea_otras", idioma,
                                         netos = paste(vapply(otras, function(m) paste0(if (neto[[m]] > 0) "+", monto_moneda(round(neto[[m]]), m)),
                                                              character(1)), collapse = ", "))))
    else ""
  meta <- paste0(.cr_e(.cr_mayus(rango)), ' &middot; ', .cr_e(.cr_sap(d$datos_sap, idioma)))

  t_cobrar <- .cr_tarjeta(.cr_t("semana.tarjeta.por_cobrar.etiqueta", idioma),
    .cr_renglones(cobros, base, .cr_C$marino, aprox = d$cobros_aprox %||% character()),
    paste0(.cr_t("semana.tarjeta.por_cobrar.explica", idioma, rango = rango), ' &middot; ',
           .cr_tn("semana.tarjeta.por_cobrar.n", as.integer(d$cobros_n %||% 0L), idioma),
           # "No incluye … · ver Cartera vencida": two keys since T46b, so
           # Panorama says the same and points to its own place.
           if (length(d$cobros_atrasados)) paste0('<br>', .cr_t("semana.tarjeta.por_cobrar.excluye", idioma,
             monto = paste(vapply(.cr_orden(d$cobros_atrasados, base), function(m) monto_moneda(d$cobros_atrasados[[m]], m),
                                  character(1)), collapse = " · ")), " · ",
             .cr_t("semana.tarjeta.por_cobrar.excluye_ver", idioma,
                   destino = tr("semana.tarjeta.cartera_vencida.etiqueta", idioma = idioma)))), "arena")
  pasivos_linea <- if (as.integer(d$pasivos_n %||% 0L) > 0) paste0(
    .cr_esp(8), '<div style="font-size:13px;color:', .cr_C$papel, ';border-top:1px solid ', .cr_C$linea_oscura, ';padding-top:8px">',
    .cr_tn("semana.tarjeta.por_pagar.pasivos", as.integer(d$pasivos_n), idioma), ' &middot; ',
    paste(vapply(.cr_orden(d$pasivos, base), function(m) .cr_m(d$pasivos[[m]], m), character(1)), collapse = " &middot; "),
    '</div>') else ""
  t_pagar <- .cr_tarjeta(.cr_t("semana.tarjeta.por_pagar.etiqueta", idioma),
    .cr_renglones(pagos, base, .cr_C$papel, aprox = d$pagos_aprox %||% character()),
    paste0(.cr_t("semana.tarjeta.por_pagar.explica", idioma, rango = rango), ' &middot; ',
           .cr_tn("semana.tarjeta.por_pagar.n", as.integer(d$pagos_n %||% 0L), idioma)),
    "marino", extra = pasivos_linea)
  t_neto <- .cr_tarjeta(.cr_t("semana.tarjeta.neto.etiqueta", idioma),
    .cr_renglones(neto, base, NULL, signo = TRUE, colorear = TRUE), .cr_t("semana.tarjeta.neto.explica", idioma))
  t_vencida <- if (!is.null(d$vencidos_pct) && !is.na(d$vencidos_pct)) {
    delta <- d$vencidos_delta
    venc <- d$vencido %||% numeric()
    .cr_tarjeta(.cr_t("semana.tarjeta.cartera_vencida.etiqueta", idioma),
      paste0('<div style="font-family:', .cr_SERIF, ';font-size:24px;line-height:1.3;color:', .cr_C$marino, '">',
             .cr_e(sprintf("%.1f%%", d$vencidos_pct)), '</div>',
             paste(vapply(.cr_orden(venc, base), function(m) paste0(
               '<div style="font-size:13px;font-weight:700;color:', .cr_C$tinta, '">',
               .cr_t("semana.tarjeta.cartera_vencida.monto", idioma, monto = monto_moneda(venc[[m]], m)), '</div>'),
               character(1)), collapse = "")),
      paste0(.cr_t("semana.tarjeta.cartera_vencida.explica", idioma, moneda = base),
             if (!is.null(delta) && !is.na(delta)) paste0(' &middot; <b style="color:', if (delta > 0) .cr_C$cobre else .cr_C$verde, '">',
               .cr_flecha(delta), ' ', .cr_t("avisos.correo.pts", idioma, pts = .cr_pts(delta)), '</b> ',
               .cr_t("avisos.correo.lunes.vs_lunes", idioma))))
  } else NULL

  lineas <- d$revision %||% correo_revision(d$alertas, idioma)
  tambien <- d$tambien %||% list()
  tambien_html <- if (length(tambien)) paste0(
    .cr_seccion(.cr_t("avisos.correo.lunes.tambien", idioma)),
    .cr_fila(.cr_tabla(.cr_vinetas(lapply(tambien, function(x) paste0(
      '<b>', .cr_t(paste0("avisos.correo.tambien.", x$tipo), idioma), '</b> ', .cr_e(x$texto)))),
      estilo = "font-size:14px;line-height:1.5"))) else ""

  cuerpo <- paste0(
    .cr_encabezado(.cr_e(.cr_mayus(correo_fecha(lunes, idioma))), .cr_t("avisos.correo.lunes.eyebrow", idioma), meta,
                   titulo, linea, idioma),
    '<tr><td style="padding:22px 0 0"></td></tr>',
    if (cxc) paste0(.cr_dos(t_cobrar, t_pagar), .cr_dos(t_neto, t_vencida)) else .cr_dos(t_pagar),
    if (cxc) .cr_revision(lineas, idioma),
    .cr_seccion(.cr_t("avisos.correo.lunes.por_dia", idioma), .cr_t("avisos.correo.lunes.por_dia_sub", idioma, base = base)),
    .cr_por_dia(d$por_dia, base, idioma, cxc),
    if (is.data.frame(d$contexto) && nrow(d$contexto)) paste0(
      .cr_seccion(.cr_t("avisos.correo.lunes.contexto", idioma), .cr_t("avisos.correo.lunes.contexto_sub", idioma, base = base)),
      .cr_contexto(d$contexto, base, idioma, cxc, d$cobertura %||% NA_real_, d$cobertura_faltan %||% as.Date(character()))),
    if (cxc && is.data.frame(d$top_cobros) && nrow(d$top_cobros)) paste0(
      .cr_seccion(.cr_t("avisos.correo.lunes.top_cobros", idioma, n = nrow(d$top_cobros))), .cr_lista(d$top_cobros, idioma)),
    if (is.data.frame(d$top_pagos) && nrow(d$top_pagos)) paste0(
      .cr_seccion(.cr_t("avisos.correo.lunes.top_pagos", idioma, n = nrow(d$top_pagos))), .cr_lista(d$top_pagos, idioma)),
    .cr_nota_orden(list(if (cxc) d$top_cobros, d$top_pagos), d$sin_tipo %||% integer(), base, idioma),
    tambien_html,
    .cr_boton_y_pie(d$url %||% "", d$datos_sap, idioma))
  .cr_documento(correo_lunes_asunto(d, idioma), cuerpo)
}

# Rows left out of the tops for lack of an exchange rate, by currency.
.avisos_sin_tipo_de <- function(...) {
  x <- unlist(lapply(list(...), function(t) attr(t, "sin_tipo")))
  if (!length(x)) return(stats::setNames(integer(), character()))
  t <- tapply(x, names(x), sum); stats::setNames(as.integer(t), names(t))
}

# ── Viernes · cómo cerró la semana ───────────────────────────────────────────
# `d`: correo_datos_viernes() (R/avisos_datos.R). Each committed item of
# Monday's plan by its state today (hecho, reprogramado, atrasado, cambio,
# por_vencer, cambio; hecho and liquidado are collected / paid "según HopDesk" and
# "según el ERP"), in a stacked bar per currency with the amounts below -- no
# headline %, which hid the composition (Luis, T45 v2). Then what to look at
# (atrasado and "salió del ERP"), the rescheduled ones, the ones moved
# several weeks in a row, what is apart from the plan (outside it,
# intercompany, plans not committed), performance, logros and radar (one
# line each), and the week ahead. Without CxC: no collections anywhere.
CORREO_MAX_LOGROS <- 2L
CORREO_MAX_RADAR  <- 3L

correo_viernes_asunto <- function(d, idioma)
  tr("avisos.correo.viernes.asunto", idioma = idioma,
     rango = correo_rango(as.Date(d$viernes) - 4L, d$viernes, idioma))

# Colors of the classes (P11: "salió" has its own neutral grey).
.cr_color_clase <- function(clase, lado) switch(clase,
  hecho = if (lado == "AR") .cr_C$dorado else .cr_C$marino, reprogramado = .cr_C$reprog, por_vencer = .cr_C$por_vencer,
  liquidado = if (lado == "AR") .cr_C$liquidado_ar else .cr_C$liquidado_ap,
  atrasado = .cr_C$cobre, cambio = .cr_C$cambio)

.cr_apilada <- function(comp) {
  total <- sum(comp$monto)
  if (total <= 0) return("")
  celdas <- paste(vapply(seq_len(nrow(comp)), function(i) {
    if (comp$monto[i] <= 0) return("")
    paste0('<td width="', max(1, round(100 * comp$monto[i] / total)), '%" style="background:', comp$color[i],
           ';height:12px;line-height:12px;font-size:1px">&nbsp;</td>')
  }, character(1)), collapse = "")
  .cr_tabla(paste0('<tr>', celdas, '</tr>'), estilo = "border-radius:3px;overflow:hidden")
}

# One side of the close: per currency, "PLAN DEL LUNES EN MXN · total", the
# stacked bar, and one line per class present (color, label · count, amount).
.cr_cierre <- function(etiqueta, explica, composicion, lado, base, idioma) {
  if (!length(composicion)) return("")
  bloques <- vapply(seq_along(composicion), function(j) {
    m <- names(composicion)[j]; comp <- composicion[[j]]
    comp$color <- vapply(comp$clase, .cr_color_clase, character(1), lado = lado)
    lineas <- paste(vapply(which(comp$n > 0), function(i) paste0(
      '<tr><td width="14" valign="top" style="padding:3px 0;color:', comp$color[i], ';font-size:13px">&#9632;</td>',
      '<td style="padding:3px 0;font-size:13px">', .cr_t(paste0("avisos.correo.clase.", comp$clase[i], ".", tolower(lado)), idioma),
      ' <span style="color:', .cr_C$gris, '">&middot; ', comp$n[i], '</span></td>',
      '<td align="right" style="padding:3px 0;font-size:13px;font-weight:700">', .cr_m(comp$monto[i], m), '</td></tr>'),
      character(1)), collapse = "")
    paste0(if (j > 1) .cr_esp(12),
           .cr_tabla(paste0('<tr><td style="font-size:12px;font-weight:900;letter-spacing:.08em;color:', .cr_C$gris, '">',
                            .cr_t("avisos.correo.viernes.plan_en", idioma, moneda = m), '</td>',
                            '<td align="right" style="font-size:12px;color:', .cr_C$gris, '">', .cr_m(sum(comp$monto), m), '</td></tr>')),
           .cr_esp(6), .cr_apilada(comp), .cr_esp(6), .cr_tabla(lineas))
  }, character(1))
  .cr_fila(.cr_tarjeta(etiqueta, paste(bloques, collapse = ""), explica))
}

.cr_sub <- function(txt, color) .cr_fila(paste0(
  '<div style="font-size:11px;font-weight:900;letter-spacing:.12em;text-transform:uppercase;color:', color,
  ';padding-top:6px">', txt, '</div>'), "0 28px 0")

# A list whose second line says the date and what happened to it:
# "vencía jue 19 nov · sin fecha nueva", "jue 19 nov → mié 9 dic".
CORREO_MAX_LISTA <- 10L   # per side; the rest is "y N más" (Luis, T45d)

.cr_lista_mov <- function(df, idioma, como, max = CORREO_MAX_LISTA) {
  if (!is.data.frame(df) || !nrow(df)) return("")
  resto <- nrow(df) - max
  if (resto > 0) df <- df[seq_len(max), , drop = FALSE]
  filas <- vapply(seq_len(nrow(df)), function(i) {
    borde <- if (i > 1) paste0("border-top:1px solid ", .cr_C$linea_tabla, ";") else ""
    f <- correo_fecha(df$fecha[i], idioma, "corta")
    detalle <- switch(como,
      atrasado = .cr_t("avisos.correo.viernes.vencia_sin_fecha", idioma, fecha = f),
      reprogramado = paste0(.cr_e(f), ' &rarr; <b>', .cr_e(correo_fecha(df$fecha_nueva[i], idioma, "corta")), '</b>'),
      racha = paste0(.cr_e(tr("avisos.correo.viernes.semanas_seguidas", idioma = idioma, n = df$semanas[i])),
                     ' &middot; ', .cr_t("avisos.correo.viernes.ahora", idioma), ' &rarr; <b>',
                     .cr_e(correo_fecha(df$fecha_nueva[i], idioma, "corta")), '</b>'))
    tipo <- if (como %in% c("reprogramado", "racha")) paste0(
      '<span style="font-size:10px;font-weight:900;letter-spacing:.1em;color:', if (identical(df$tipo[i], "AR")) .cr_C$arena_tinta else .cr_C$marino,
      '">', .cr_t(if (identical(df$tipo[i], "AR")) "avisos.correo.tipo.cobro" else "avisos.correo.tipo.pago", idioma), '</span> &middot; ') else ""
    paste0('<tr><td style="', borde, 'padding:9px 0">',
           .cr_tabla(paste0('<tr><td style="font-size:14px;font-weight:700">', .cr_e(df$contraparte[i]), '</td>',
                            '<td align="right" valign="top" style="font-size:14px;font-weight:700;color:', .cr_C$marino, ';padding-left:10px">',
                            .cr_m(df$monto[i], df$moneda[i]), '</td></tr>',
                            '<tr><td colspan="2" style="font-size:12px;color:', .cr_C$gris, ';padding-top:2px">', tipo,
                            .cr_e(df$empresa[i]), ' &middot; ', detalle, '</td></tr>')), '</td></tr>')
  }, character(1))
  paste0(.cr_fila(.cr_tabla(paste(filas, collapse = "")), "0 28px 4px"),
         if (resto > 0) .cr_nota(.cr_t("avisos.correo.viernes.y_mas", idioma, n = resto), "0 28px 6px"))
}

.cr_renglon <- function(etiqueta, texto) paste0(
  '<tr><td style="border-top:1px solid ', .cr_C$linea_tabla, ';padding:10px 0">',
  '<div style="font-size:14px;font-weight:700;color:', .cr_C$tinta, '">', etiqueta, '</div>',
  '<div style="font-size:13px;line-height:1.5;color:', .cr_C$gris, ';padding-top:2px">', texto, '</div></td></tr>')

.cr_montos_txt <- function(x, base, signo = "") paste(vapply(.cr_orden(x, base), function(m)
  paste0(signo, monto_moneda(x[[m]], m)), character(1)), collapse = " · ")

.cr_desempeno <- function(des, cxc, base, idioma) {
  if (is.null(des)) return("")
  fila <- function(clave, x) {
    if (is.null(x)) return("")
    ref <- if (!is.na(x$semana_pasada)) x$semana_pasada else x$mediana
    juicio <- if (is.na(ref) || abs(x$hoy - ref) < 0.005) "" else {
      mejor <- x$hoy < ref   # less left atrasado is better
      paste0('<b style="color:', if (mejor) .cr_C$verde else .cr_C$cobre, '">', .cr_flecha(x$hoy - ref), ' ',
             .cr_t(if (mejor) "avisos.correo.desempeno.mejor" else "avisos.correo.desempeno.peor", idioma), '</b> &middot; ')
    }
    detalle <- paste0(.cr_tn("avisos.correo.desempeno.partidas", as.integer(x$hoy_n), idioma), ' &middot; ',
      if (isTRUE(des$ilegible)) .cr_t("avisos.correo.desempeno.ilegible", idioma)
      else if (isTRUE(des$sin_historia)) .cr_t("avisos.correo.desempeno.sin_historia", idioma)
      else .cr_t("avisos.correo.desempeno.detalle", idioma,
                 semana = if (is.na(x$semana_pasada)) "—" else monto_moneda(x$semana_pasada, base),
                 mediana = if (is.na(x$mediana)) "—" else monto_moneda(x$mediana, base)))
    paste0('<tr><td style="border-top:1px solid ', .cr_C$linea_tabla, ';padding:11px 0">',
           .cr_tabla(paste0('<tr><td style="font-size:14px;font-weight:700">', .cr_t(clave, idioma), '</td>',
                            '<td align="right" style="font-family:', .cr_SERIF, ';font-size:19px;color:', .cr_C$marino,
                            ';padding-left:10px">', .cr_m(x$hoy, base), '</td></tr>',
                            '<tr><td colspan="2" style="font-size:12px;line-height:1.5;color:', .cr_C$gris, ';padding-top:2px">',
                            juicio, detalle, '</td></tr>')), '</td></tr>')
  }
  .cr_fila(.cr_tabla(paste0(if (cxc) fila("avisos.correo.desempeno.cobros", des$cobros),
                            fila("avisos.correo.desempeno.pagos", des$pagos)),
                     estilo = paste0("border-bottom:1px solid ", .cr_C$linea_tabla)), "0 28px 4px")
}

correo_viernes_html <- function(d, dest) .correo_con_tema(d$tema, function() .correo_viernes_cuerpo(d, dest))
.correo_viernes_cuerpo <- function(d, dest) {
  idioma <- dest$idioma %||% "es"; cxc <- isTRUE(dest$ver_cxc)
  base <- d$moneda_base %||% MONEDA_BASE_OMISION
  viernes <- as.Date(d$viernes); lunes <- viernes - 4L
  rango <- .cr_del_al(lunes, viernes, idioma)
  t <- d$titular %||% list()
  mb <- function(x) monto_moneda(round(as.numeric(x %||% 0)), base)
  titulo <- if (cxc && isTRUE(t$hay_cobros))
      .cr_t("avisos.correo.viernes.titulo", idioma, cobrado = mb(t$cobrado), reprog = mb(t$cobros_reprog), atrasado = mb(t$cobros_atrasado))
    else if (isTRUE(t$hay_pagos))
      .cr_t("avisos.correo.viernes.titulo.pagos", idioma, pagado = mb(t$pagado), reprog = mb(t$pagos_reprog), atrasado = mb(t$pagos_atrasado))
    else .cr_t("avisos.correo.viernes.titulo.sin_plan", idioma)
  corte <- d$corte
  if (!isTRUE(d$sin_cierre) && !is.null(corte) && !is.na(corte) && as.Date(corte) < viernes)
    titulo <- paste0(.cr_t("avisos.correo.viernes.al_corte", idioma, fecha = correo_fecha(corte, idioma, "dia")), " ",
                     tolower(substr(titulo, 1, 1)), substring(titulo, 2))
  linea <- paste0(
    if (cxc && isTRUE(t$hay_cobros) && isTRUE(t$hay_pagos)) paste0(.cr_t("avisos.correo.viernes.linea_pagos", idioma,
      pagado = mb(t$pagado), reprog = mb(t$pagos_reprog), atrasado = mb(t$pagos_atrasado)), " "),
    .cr_t("avisos.correo.viernes.linea", idioma))
  meta <- paste0(.cr_e(.cr_mayus(rango)), ' &middot; ', .cr_e(.cr_sap(d$datos_sap, idioma)))

  cierre <- paste0("", 
    if (cxc) .cr_cierre(.cr_t("semana.tarjeta.cierre_cobros.etiqueta", idioma), .cr_t("semana.tarjeta.cierre_cobros.explica", idioma),
                        d[["cobros"]], "AR", base, idioma),
    .cr_cierre(.cr_t("semana.tarjeta.cierre_pagos.etiqueta", idioma), .cr_t("semana.tarjeta.cierre_pagos.explica", idioma),
               d[["pagos"]], "AP", base, idioma))
  conc <- Filter(function(x) cxc || identical(x$tipo, "AP"), d$concentracion %||% list())
  conc_html <- if (length(conc)) .cr_fila(.cr_tabla(paste0(
    '<tr><td style="padding:12px 16px;border-radius:12px;background:', .cr_C$revision, ';color:', .cr_C$revision_tinta,
    ';font-size:14px;line-height:1.5"><b>', .cr_t("avisos.correo.viernes.concentracion", idioma), '</b> ',
    paste(vapply(conc, function(x) .cr_t(paste0("avisos.correo.viernes.concentracion_", tolower(x$tipo)), idioma,
      contraparte = x$contraparte, pct = sprintf("%.0f%%", x$pct), moneda = x$moneda), character(1)), collapse = " "),
    '</td></tr>')), "0 28px 6px") else ""

  # Lo que hay que ver: what stayed atrasado (the ERP's settlements count as
  # collected / paid, P3).
  ver <- paste0("", 
    if (cxc && nrow(d$atrasados_cobros %||% data.frame())) paste0(.cr_sub(.cr_t("avisos.correo.viernes.atrasados_cobros", idioma), .cr_C$arena_tinta),
                                                                .cr_lista_mov(d$atrasados_cobros, idioma, "atrasado")),
    if (nrow(d$atrasados_pagos %||% data.frame())) paste0(.cr_sub(.cr_t("avisos.correo.viernes.atrasados_pagos", idioma), .cr_C$marino),
                                                       .cr_lista_mov(d$atrasados_pagos, idioma, "atrasado")))
  ver_html <- paste0(.cr_seccion(.cr_t("avisos.correo.viernes.ver", idioma), .cr_t("avisos.correo.viernes.ver_sub", idioma)),
                     if (nzchar(ver)) ver else .cr_nota(.cr_t("avisos.correo.viernes.nada_que_ver", idioma)))

  lado <- function(df, clave, color, como) if (is.data.frame(df) && nrow(df))
    paste0(.cr_sub(.cr_t(clave, idioma), color), .cr_lista_mov(df, idioma, como)) else ""
  rep <- paste0("", if (cxc) lado(d$reprog_cobros, "semana.tarjeta.cierre_cobros.etiqueta", .cr_C$arena_tinta, "reprogramado"),
                lado(d$reprog_pagos, "semana.tarjeta.cierre_pagos.etiqueta", .cr_C$marino, "reprogramado"))
  rep_html <- if (nzchar(rep)) paste0(
    .cr_seccion(.cr_t("avisos.correo.viernes.reprogramados", idioma), .cr_t("avisos.correo.viernes.reprogramados_sub", idioma)),
    rep) else ""

  ra <- d$racha %||% list(sin_historia = TRUE)
  rf <- ra$filas
  if (!cxc && is.data.frame(rf)) rf <- rf[rf$tipo %in% "AP", , drop = FALSE]
  racha_html <- paste0(.cr_seccion(.cr_t("avisos.correo.viernes.racha", idioma, n = ra$minimo),
                                   .cr_t("avisos.correo.viernes.racha_sub", idioma)),
    if (isTRUE(ra$ilegible)) .cr_nota(.cr_t("avisos.correo.viernes.racha_ilegible", idioma))
    else if (isTRUE(ra$sin_historia)) .cr_nota(.cr_t("avisos.correo.viernes.racha_sin_historia", idioma))
    else if (is.data.frame(rf) && nrow(rf)) paste0("",
      if (cxc) lado(rf[rf$tipo %in% "AR", , drop = FALSE], "semana.tarjeta.cierre_cobros.etiqueta", .cr_C$arena_tinta, "racha"),
      lado(rf[rf$tipo %in% "AP", , drop = FALSE], "semana.tarjeta.cierre_pagos.etiqueta", .cr_C$marino, "racha"))
    else .cr_nota(.cr_t("avisos.correo.viernes.racha_ninguna", idioma, n = ra$minimo)))

  fu <- d$fuera %||% list(); pl <- d$planes %||% list(n = 0L)
  aparte <- paste0("", 
    if ((cxc && length(fu$cobros)) || length(fu$pagos)) .cr_renglon(.cr_t("avisos.correo.viernes.fuera", idioma), paste(c(
      if (cxc && length(fu$cobros)) .cr_t("avisos.correo.viernes.fuera_cobros", idioma, montos = .cr_montos_txt(fu$cobros, base, "+"), n = fu$cobros_n),
      if (length(fu$pagos)) .cr_t("avisos.correo.viernes.fuera_pagos", idioma, montos = .cr_montos_txt(fu$pagos, base, "−"), n = fu$pagos_n)),
      collapse = " ")),
    if (as.integer(pl$n %||% 0L) > 0) .cr_renglon(.cr_t("avisos.correo.viernes.planes", idioma), paste0(
      .cr_t("avisos.correo.viernes.planes_texto", idioma, n = pl$n, montos = .cr_montos_txt(pl$montos, base),
            requisicion = pl$requisicion, manual = pl$manual),
      if (as.integer(pl$pagados %||% 0L) > 0) paste0(" ", .cr_tn("avisos.correo.viernes.planes_pagados", as.integer(pl$pagados), idioma)))),
    if (as.integer(fu$ic_n %||% 0L) > 0) .cr_renglon(.cr_t("avisos.correo.viernes.ic", idioma),
      .cr_e(tr_n("avisos.correo.viernes.ic_texto", as.integer(fu$ic_n), montos = .cr_montos_txt(fu$ic, base), idioma = idioma))))
  aparte_html <- if (nzchar(aparte)) paste0(.cr_seccion(.cr_t("avisos.correo.viernes.aparte", idioma)),
    .cr_fila(.cr_tabla(aparte, estilo = paste0("border-bottom:1px solid ", .cr_C$linea_tabla)), "0 28px 4px")) else ""

  des_html <- paste0(.cr_seccion(.cr_t("avisos.correo.viernes.desempeno", idioma), .cr_t("avisos.correo.viernes.desempeno_sub", idioma)),
                     .cr_desempeno(d$desempeno, cxc, base, idioma), .cr_nota(.cr_t("avisos.correo.viernes.desempeno_nota", idioma)))

  # Logros and radar, one line each (P13).
  logros <- utils::head(as.character(d$logros %||% (if (is.null(d$items)) character() else correo_items_texto(d$items, "logro", dest, CORREO_MAX_LOGROS))),
                        CORREO_MAX_LOGROS)
  radar <- utils::head(as.character(d$radar %||% (if (is.null(d$items)) character() else correo_items_texto(d$items, "radar", dest, CORREO_MAX_RADAR))),
                       CORREO_MAX_RADAR)
  breve <- function(clave, lineas, simbolo) if (length(lineas)) .cr_fila(paste0(
    '<div style="font-size:13px;line-height:1.6"><b style="color:', .cr_C$logro_tinta, '">', simbolo, ' ', .cr_t(clave, idioma), ':</b> ',
    paste(.cr_e(lineas), collapse = " &middot; "), '</div>'), "4px 28px 0") else ""
  breves <- paste0(breve("avisos.correo.viernes.logros", logros, "&#9733;"), breve("avisos.correo.viernes.radar", radar, "&#9670;"))

  s <- d$siguiente %||% list()
  sl <- as.Date(s$lunes %||% (lunes + 7L))
  sig_cobrar <- .cr_tarjeta(.cr_t("semana.tarjeta.por_cobrar.etiqueta_siguiente", idioma),
    .cr_renglones(s$cobros %||% numeric(), base, .cr_C$marino, 20),
    .cr_tn("semana.tarjeta.por_cobrar.n", as.integer(s$cobros_n %||% 0L), idioma), "arena")
  sig_pagar <- .cr_tarjeta(.cr_t("semana.tarjeta.por_pagar.etiqueta_siguiente", idioma),
    .cr_renglones(s$pagos %||% numeric(), base, .cr_C$papel, 20),
    .cr_tn("semana.tarjeta.por_pagar.n", as.integer(s$pagos_n %||% 0L), idioma), "marino")
  sig_neto <- .cr_tarjeta(.cr_t("semana.tarjeta.neto.etiqueta_proyectado", idioma),
    .cr_renglones(.cr_neto(s$cobros %||% numeric(), s$pagos %||% numeric(), base), base, NULL, 20, signo = TRUE, colorear = TRUE),
    .cr_t("semana.tarjeta.neto.explica", idioma))
  tc <- if (cxc) s$top_cobros else NULL; tp <- s$top_pagos
  sig_html <- paste0(
    .cr_seccion(.cr_t("avisos.correo.viernes.siguiente", idioma), .cr_e(.cr_mayus(.cr_del_al(sl, sl + 4L, idioma)))),
    if (cxc) paste0(.cr_dos(sig_cobrar, sig_pagar), .cr_fila(sig_neto)) else .cr_dos(sig_pagar),
    if (is.data.frame(tc) && nrow(tc)) paste0(.cr_seccion(.cr_t("avisos.correo.lunes.top_cobros", idioma, n = nrow(tc))),
                                              .cr_lista(tc, idioma)),
    if (is.data.frame(tp) && nrow(tp)) paste0(.cr_seccion(.cr_t("avisos.correo.lunes.top_pagos", idioma, n = nrow(tp))),
                                              .cr_lista(tp, idioma)),
    .cr_nota_orden(list(tc, tp), .avisos_sin_tipo_de(tc, tp), base, idioma))

  # SAP not refreshed since Monday: no close at all, only the week ahead.
  if (isTRUE(d$sin_cierre)) {
    titulo <- .cr_t("avisos.correo.viernes.titulo.sin_cierre", idioma)
    linea <- .cr_t("avisos.correo.viernes.linea_sin_cierre", idioma)
  }
  cuerpo <- paste0(
    .cr_encabezado(.cr_e(.cr_mayus(correo_fecha(viernes, idioma))), .cr_t("avisos.correo.viernes.eyebrow", idioma), meta,
                   titulo, linea, idioma),
    if (isTRUE(d$sin_cierre)) sig_html else paste0(
      .cr_seccion(.cr_t("avisos.correo.viernes.cierre", idioma), .cr_t("avisos.correo.viernes.cierre_sub", idioma), "22px 28px 10px"),
      cierre, conc_html, ver_html, rep_html, racha_html, aparte_html, des_html, breves, sig_html),
    .cr_boton_y_pie(d$url %||% "", d$datos_sap, idioma))
  .cr_documento(correo_viernes_asunto(d, idioma), cuerpo)
}
