# =============================================================================
# R/avisos_correos.R
# The two weekly emails of Panorama (frente D): Monday "Panorama de la
# semana" (8:00) and Friday "Cierre de semana" (17:00), approved by Luis on
# 2026-11-11 from the mockup (requisiciones/D_mockup_correos.html).
#
#   correo_destinatarios()  who gets them: live client users with a valid
#                           email who hold can_receive_weekly_email (dev and
#                           admin by default, others per user) and did not
#                           turn them off (preference correos_semana = FALSE)
#   correo_lunes_html()     the Monday email, from already computed data
#   correo_viernes_html()   the Friday email: no absolute amounts, only %,
#                           points, days and counts (Luis)
#
# Email HTML: inline styles and tables only (Gmail and Outlook drop <style>,
# flex and SVG). Fraunces shows where the client loads web fonts (Apple
# Mail, iOS); elsewhere Georgia. Every visible text comes from the
# dictionary (avisos.correo.*) in the recipient's language; dynamic text is
# HTML-escaped. A text marked *like this* in the dictionary is set in italic.
#
# Never prints: the job runs in a public repo's logs (see avisos_job.R).
# =============================================================================

CORREO_COLORES  <- list(
  fondo = "#EDEBE4", papel = "#FFFFFF", tinta = "#1A2233", gris = "#5A6474",
  marino = "#13213A", dorado = "#C9AE6E", dorado_osc = "#B0924F", linea = "#E4E1D8",
  linea_tabla = "#EFECE4", pie = "#F7F6F2", arena = "#EFE6D2", arena_tinta = "#6B5320",
  gris_claro = "#9AA6BA", cobre = "#9A4524", revision = "#F7E9E1", revision_tinta = "#7C3519",
  verde = "#2F6B4F", logro = "#FBF6EA", logro_borde = "#E9DBB7", logro_tinta = "#8A6D2C")
.CORREO_EMAIL_RE <- "^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$"

# ── Destinatarios ────────────────────────────────────────────────────────────

# `usuarios`: auth_load_usuarios(cid). `config`: the client's tiers_config.rds
# (or NULL). `prefs`: preferencias.rds as a data frame (usuario, datos).
# Returns one row per recipient: usuario, email, tier, idioma, ver_cxc.
# Pure: the job reads the three files (strictly) and passes them in.
correo_destinatarios <- function(usuarios, config = NULL, prefs = NULL) {
  vacio <- data.frame(usuario = character(), email = character(), tier = character(),
                      idioma = character(), ver_cxc = logical(), stringsAsFactors = FALSE)
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
    p <- .correo_prefs(prefs, user[i])
    if (identical(p$correos_semana, FALSE)) return(NULL)
    idioma <- p$idioma %||% "es"
    data.frame(usuario = user[i], email = email[i], tier = tier[i],
               idioma = if (idioma %in% i18n_idiomas()) idioma else "es",
               ver_cxc = is.null(nivel_calendario(tier[i])), stringsAsFactors = FALSE)
  })
  rbind(vacio, do.call(rbind, filas))
}

.correo_prefs <- function(prefs, usuario) {
  if (!is.data.frame(prefs) || !nrow(prefs)) return(list())
  i <- which(tolower(prefs$usuario) == usuario)
  if (!length(i)) return(list())
  tryCatch(as.list(jsonlite::fromJSON(prefs$datos[i[1]], simplifyVector = TRUE)), error = function(e) list())
}

# ── Formato ──────────────────────────────────────────────────────────────────

.cr_e <- function(x) htmltools::htmlEscape(as.character(x))
# Dictionary text, escaped, with *this* in italic. Placeholders are filled
# before escaping, so a value can never inject markup.
.cr_t <- function(clave, idioma, ...) {
  txt <- .cr_e(tr(clave, idioma = idioma, ...))
  gsub("\\*([^*]+)\\*", "<i>\\1</i>", txt)
}

correo_monto <- function(x, moneda = "MXN", moneda_base = "MXN") {
  n <- formatC(round(as.numeric(x), 2), format = "f", digits = 2, big.mark = ",")
  if (identical(moneda, moneda_base)) paste0("$", n) else paste0("$", n, " ", moneda)
}
.cr_pts <- function(x) sprintf("%.1f", abs(x))

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

correo_hora <- function(t, idioma) {
  if (is.null(t) || is.na(t)) return("")
  lt <- as.POSIXct(t, tz = "UTC"); attr(lt, "tzone") <- UMBRAL_TZ
  tr("avisos.correo.fecha_hora", idioma = idioma,
     fecha = correo_fecha(as.Date(format(lt, "%Y-%m-%d")), idioma, "dia"), hora = format(lt, "%H:%M"))
}

# ── Piezas del correo (tablas, estilos en línea) ─────────────────────────────

.cr_C <- CORREO_COLORES
.cr_SERIF <- "Fraunces,Georgia,serif"
.cr_SANS  <- "Lato,Arial,sans-serif"

.cr_tabla <- function(..., estilo = "") paste0(
  '<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0" style="', estilo, '">',
  ..., '</table>')

.cr_encabezado <- function(fecha_txt, eyebrow, titulo) paste0(
  '<tr><td style="background:', .cr_C$marino, ';padding:28px 30px 26px">',
  .cr_tabla('<tr><td style="font-weight:900;font-size:12px;letter-spacing:.26em;color:', .cr_C$dorado, '">&#9672;&nbsp; ',
         eyebrow$marca, '</td><td align="right" style="font-size:12px;color:', .cr_C$gris_claro, '">', fecha_txt,
         '</td></tr>'),
  '<div style="height:14px;line-height:14px">&nbsp;</div>',
  '<div style="font-size:11px;font-weight:900;letter-spacing:.22em;color:', .cr_C$dorado, '">', eyebrow$texto, '</div>',
  '<div style="height:10px;line-height:10px">&nbsp;</div>',
  '<div style="font-family:', .cr_SERIF, ';font-weight:500;font-size:30px;line-height:1.15;color:#FFFFFF">',
  titulo, '</div></td></tr>')

# A tile: fondo/borde, label color, value (already HTML), optional detail.
.cr_mosaico <- function(etiqueta, valor, estilo = c("arena", "marino", "borde"), detalle = NULL, chico = FALSE) {
  estilo <- match.arg(estilo)
  caja <- switch(estilo, arena = paste0("background:", .cr_C$arena),
                 marino = paste0("background:", .cr_C$marino),
                 borde = paste0("border:1px solid ", .cr_C$linea))
  c_eti <- switch(estilo, arena = .cr_C$arena_tinta, marino = .cr_C$dorado, borde = .cr_C$gris)
  c_val <- if (estilo == "marino") "#FFFFFF" else .cr_C$marino
  c_det <- switch(estilo, arena = .cr_C$arena_tinta, marino = .cr_C$gris_claro, borde = .cr_C$gris)
  .cr_tabla(paste0('<tr><td style="', caja, ';border-radius:14px;padding:', if (chico) "14px 14px 12px" else "16px", '">',
                '<div style="font-size:', if (chico) 10 else 11, 'px;font-weight:900;letter-spacing:.12em;color:', c_eti, '">',
                etiqueta, '</div>',
                '<div style="font-family:', .cr_SERIF, ';font-size:', if (chico) 26 else 24, 'px;color:', c_val, '">', valor, '</div>',
                if (!is.null(detalle)) paste0('<div style="font-size:12px;color:', c_det, '">', detalle, '</div>'),
                '</td></tr>'))
}

# Tiles in a grid of `por_fila` columns.
.cr_rejilla <- function(mosaicos, por_fila) {
  if (!length(mosaicos)) return("")
  filas <- split(mosaicos, ceiling(seq_along(mosaicos) / por_fila))
  ancho <- paste0(floor(100 / por_fila), "%")
  paste0('<tr><td style="padding:24px 30px 4px">', .cr_tabla(paste0(vapply(filas, function(f) {
    celdas <- vapply(seq_along(f), function(j) paste0(
      '<td width="', ancho, '" valign="top" style="padding:0 ', if (j < length(f)) 5 else 0, 'px 10px ',
      if (j > 1) 5 else 0, 'px">', f[[j]], '</td>'), character(1))
    relleno <- if (length(f) < por_fila) paste(rep(paste0('<td width="', ancho, '"></td>'), por_fila - length(f)), collapse = "") else ""
    paste0("<tr>", paste(celdas, collapse = ""), relleno, "</tr>")
  }, character(1)), collapse = "")), '</td></tr>')
}

.cr_titulo_seccion <- function(txt, pad = "20px 30px 6px") paste0(
  '<tr><td style="padding:', pad, '"><div style="font-family:', .cr_SERIF,
  ';font-weight:500;font-size:20px;color:', .cr_C$marino, ';padding-bottom:10px">', txt, '</div>')

# Top-5 table: df with contraparte, empresa, fecha, monto, moneda.
.cr_tabla_top <- function(titulo, df, idioma, moneda_base) {
  if (!is.data.frame(df) || !nrow(df)) return("")
  th <- function(x, d = "") paste0('<td', d, ' style="padding:6px 0;font-weight:900">', .cr_t(paste0("avisos.correo.col.", x), idioma), '</td>')
  td <- 'border-top:1px solid #EFECE4'
  filas <- vapply(seq_len(nrow(df)), function(i) paste0(
    '<tr><td style="padding:9px 8px 9px 0;', td, '">', .cr_e(df$contraparte[i]), '</td>',
    '<td style="', td, '">', .cr_e(df$empresa[i]), '</td>',
    '<td style="', td, '">', .cr_e(correo_fecha(df$fecha[i], idioma, "corta")), '</td>',
    '<td align="right" style="', td, ';font-weight:700">', .cr_e(correo_monto(df$monto[i], df$moneda[i], moneda_base)), '</td></tr>'),
    character(1))
  paste0(.cr_titulo_seccion(titulo),
         .cr_tabla(paste0('<tr style="color:', .cr_C$gris, ';font-size:11px;letter-spacing:.1em">',
                       th("contraparte"), th("empresa"), th("fecha"), th("monto", ' align="right"'), '</tr>',
                       paste(filas, collapse = "")),
                estilo = "border-collapse:collapse;font-size:13px"),
         '</td></tr>')
}

# Bulleted lines (already HTML), gold diamond or star.
.cr_vinetas <- function(lineas, simbolo = "&#9670;", tam = "10px") {
  paste(vapply(seq_along(lineas), function(i) paste0(
    '<tr><td width="18" valign="top" style="color:', .cr_C$dorado_osc, ';font-size:', tam, ';padding-top:4px">', simbolo, '</td>',
    '<td style="padding-bottom:', if (i < length(lineas)) 8 else 0, 'px">', lineas[[i]], '</td></tr>'),
    character(1)), collapse = "")
}

.cr_boton_y_pie <- function(url, datos_sap, idioma) paste0(
  '<tr><td align="center" style="padding:22px 30px 30px">',
  '<a href="', .cr_e(url), '" style="display:inline-block;padding:15px 32px;border-radius:999px;background:', .cr_C$marino,
  ';color:#FFFFFF;font-weight:900;font-size:15px;text-decoration:none">', .cr_t("avisos.correo.boton", idioma),
  ' <span style="color:', .cr_C$dorado, '">&rarr;</span></a></td></tr>',
  '<tr><td style="background:', .cr_C$pie, ';border-top:1px solid ', .cr_C$linea, ';padding:16px 30px;font-size:12px;color:', .cr_C$gris, '">',
  .cr_tabla(paste0('<tr><td>', if (!is.null(datos_sap) && !is.na(datos_sap))
    .cr_t("avisos.correo.pie.datos", idioma, fecha = correo_hora(datos_sap, idioma)) else "",
    '</td><td align="right">', .cr_t("avisos.correo.pie.apagar", idioma), '</td></tr>')),
  '</td></tr>')

.cr_documento <- function(asunto, cuerpo) paste0(
  '<!DOCTYPE html><html><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width,initial-scale=1">',
  '<title>', .cr_e(asunto), '</title>',
  '<link href="https://fonts.googleapis.com/css2?family=Fraunces:ital,wght@0,400;0,500;1,400&amp;family=Lato:wght@400;700;900&amp;display=swap" rel="stylesheet">',
  '</head><body style="margin:0;padding:0;background:', .cr_C$fondo, '">',
  '<table role="presentation" width="100%" cellpadding="0" cellspacing="0" border="0" style="background:', .cr_C$fondo,
  ';font-family:', .cr_SANS, ';color:', .cr_C$tinta, '"><tr><td align="center" style="padding:24px 16px">',
  '<table role="presentation" width="568" cellpadding="0" cellspacing="0" border="0" style="width:568px;max-width:100%;background:',
  .cr_C$papel, ';border-radius:18px;overflow:hidden">', cuerpo, '</table></td></tr></table></body></html>')

# Several currencies in one tile: the base one large, the rest below it.
.cr_montos <- function(montos, moneda_base) {
  if (!length(montos)) return(.cr_e(correo_monto(0, moneda_base, moneda_base)))
  orden <- c(intersect(moneda_base, names(montos)), setdiff(names(montos), moneda_base))
  txt <- vapply(orden, function(m) .cr_e(correo_monto(montos[[m]], m, moneda_base)), character(1))
  if (length(txt) == 1) return(txt[[1]])
  paste0(txt[[1]], paste0('<div style="font-family:', .cr_SANS, ';font-size:13px;font-weight:700">',
                          txt[-1], '</div>', collapse = ""))
}

.cr_montos_linea <- function(montos, moneda_base) {
  if (!length(montos)) return(correo_monto(0, moneda_base, moneda_base))
  orden <- c(intersect(moneda_base, names(montos)), setdiff(names(montos), moneda_base))
  paste(vapply(orden, function(m) correo_monto(montos[[m]], m, moneda_base), character(1)), collapse = " + ")
}
.cr_mayus <- function(s) paste0(toupper(substr(s, 1, 1)), substring(s, 2))

.cr_flecha <- function(delta) if (delta > 0) "&#9650;" else if (delta < 0) "&#9660;" else "&#8226;"

# ── Lunes · Panorama de la semana ────────────────────────────────────────────
# `d`: list(
#   lunes            Date of the Monday
#   cobros, pagos    named numeric by moneda (expected this week)
#   pasivos_n, pasivos  count and named numeric by moneda
#   vencidos_pct, vencidos_delta   client-level CxC % past due and its Δ (pts; NA = no history)
#   revision         character: "Para revisión" lines (umbral alerts, CxC)
#   top_cobros, top_pagos   data frames (contraparte, empresa, fecha, monto, moneda)
#   tambien          list of list(tipo = "pasivos"|"solicitud"|"comunicado", texto)
#   datos_sap        POSIXct of the snapshot
#   moneda_base      default "MXN"
#   url              the app's address)
# `dest`: one row of correo_destinatarios() (idioma, ver_cxc).
correo_lunes_asunto <- function(d, idioma)
  tr("avisos.correo.lunes.asunto", idioma = idioma,
     rango = correo_rango(d$lunes, as.Date(d$lunes) + 4L, idioma))

correo_lunes_html <- function(d, dest) {
  idioma <- dest$idioma %||% "es"; cxc <- isTRUE(dest$ver_cxc)
  base <- d$moneda_base %||% "MXN"
  # The headline compares the base currency only: amounts in different
  # currencies are never added up (integrador, 2026-11-12).
  en_base <- function(x) { v <- if (base %in% names(x)) as.numeric(x[[base]]) else NA; if (is.na(v)) 0 else v }
  titulo <- if (!cxc) .cr_t("avisos.correo.lunes.titulo.sin_cxc", idioma)
            else if (en_base(d$cobros) >= en_base(d$pagos)) .cr_t("avisos.correo.lunes.titulo.cobros", idioma)
            else .cr_t("avisos.correo.lunes.titulo.pagos", idioma)
  m <- list()
  if (cxc) m <- c(m, list(.cr_mosaico(.cr_t("avisos.correo.lunes.mosaico.cobros", idioma), .cr_montos(d$cobros, base), "arena")))
  m <- c(m, list(.cr_mosaico(.cr_t("avisos.correo.lunes.mosaico.pagos", idioma), .cr_montos(d$pagos, base), "marino")))
  m <- c(m, list(.cr_mosaico(.cr_t("avisos.correo.lunes.mosaico.pasivos", idioma),
                          .cr_t("avisos.correo.lunes.pasivos_valor", idioma, n = as.integer(d$pasivos_n %||% 0L),
                             monto = .cr_montos_linea(d$pasivos, base)),
                          "borde")))
  if (cxc && !is.null(d$vencidos_pct) && !is.na(d$vencidos_pct)) {
    delta <- d$vencidos_delta
    val <- paste0(.cr_e(sprintf("%.1f%%", d$vencidos_pct)),
                  if (!is.null(delta) && !is.na(delta)) paste0(
                    ' <span style="font-family:', .cr_SANS, ';font-size:13px;font-weight:900;color:',
                    if (delta > 0) .cr_C$cobre else .cr_C$verde, '">', .cr_flecha(delta), ' ',
                    .cr_t("avisos.correo.pts", idioma, pts = .cr_pts(delta)), '</span>'))
    m <- c(m, list(.cr_mosaico(.cr_t("avisos.correo.lunes.mosaico.vencidos", idioma), val, "borde")))
  }
  revision <- if (cxc && length(d$revision)) paste0(
    '<tr><td style="padding:14px 30px 0">', .cr_tabla(paste0(
      '<tr><td style="padding:14px 16px;border-radius:14px;background:', .cr_C$revision, ';color:', .cr_C$revision_tinta,
      ';font-size:14px;line-height:1.5"><b>', .cr_t("avisos.correo.lunes.para_revision", idioma), '</b> ',
      paste(.cr_e(d$revision), collapse = "<br>"), '</td></tr>')), '</td></tr>') else ""
  tambien <- d$tambien %||% list()
  tambien_html <- if (length(tambien)) paste0(
    .cr_titulo_seccion(.cr_t("avisos.correo.lunes.tambien", idioma), "20px 30px"),
    .cr_tabla(.cr_vinetas(lapply(tambien, function(x) paste0(
      '<b>', .cr_t(paste0("avisos.correo.tambien.", x$tipo), idioma), '</b> ', .cr_e(x$texto)))),
      estilo = "font-size:14px;line-height:1.5"), '</td></tr>') else ""
  cuerpo <- paste0(
    .cr_encabezado(.cr_e(.cr_mayus(correo_fecha(d$lunes, idioma))),
                list(marca = .cr_t("avisos.correo.marca", idioma), texto = .cr_t("avisos.correo.lunes.eyebrow", idioma)), titulo),
    .cr_rejilla(m, 2), revision,
    if (cxc) .cr_tabla_top(.cr_t("avisos.correo.lunes.top_cobros", idioma), d$top_cobros, idioma, base) else "",
    .cr_tabla_top(.cr_t("avisos.correo.lunes.top_pagos", idioma), d$top_pagos, idioma, base),
    tambien_html,
    .cr_boton_y_pie(d$url %||% "", d$datos_sap, idioma))
  .cr_documento(correo_lunes_asunto(d, idioma), cuerpo)
}

# ── Viernes · Cierre de semana ───────────────────────────────────────────────
# No absolute amounts anywhere (Luis). `d`: list(
#   viernes          Date of the Friday
#   pagos_conf, pagos_total     payments of the week confirmed / scheduled (counts)
#   cobros_pct       % of the week's expected collections received (NA = none expected)
#   vencidos_pct, vencidos_delta   client-level CxC % past due and its Δ over the week
#   logros           character, already in the recipient's language (at most 2 shown)
#   radar            character, the same ("En el radar", at most 3 shown)
#   datos_sap, url)
# Logros and radar lines that speak of CxC are left out by the caller for
# recipients without CxC; this function drops the CxC tiles for them.
CORREO_MAX_LOGROS <- 2L
CORREO_MAX_RADAR  <- 3L

correo_viernes_asunto <- function(d, idioma)
  tr("avisos.correo.viernes.asunto", idioma = idioma,
     rango = correo_rango(as.Date(d$viernes) - 4L, d$viernes, idioma))

correo_viernes_html <- function(d, dest) {
  idioma <- dest$idioma %||% "es"; cxc <- isTRUE(dest$ver_cxc)
  total <- as.integer(d$pagos_total %||% 0L); conf <- as.integer(d$pagos_conf %||% 0L)
  pct_pagos <- if (total > 0) round(100 * conf / total) else NA
  titulo <- if (is.na(pct_pagos)) .cr_t("avisos.correo.viernes.titulo.sin_pagos", idioma)
            else .cr_t("avisos.correo.viernes.titulo", idioma, pct = pct_pagos)
  m <- list()
  if (!is.na(pct_pagos))
    m <- c(m, list(.cr_mosaico(.cr_t("avisos.correo.viernes.mosaico.pagos", idioma), paste0(pct_pagos, "%"), "marino",
                            .cr_t("avisos.correo.viernes.pagos_detalle", idioma, n = conf, total = total), chico = TRUE)))
  if (cxc && !is.null(d$cobros_pct) && !is.na(d$cobros_pct))
    m <- c(m, list(.cr_mosaico(.cr_t("avisos.correo.viernes.mosaico.cobros", idioma), paste0(round(d$cobros_pct), "%"), "arena",
                            .cr_t("avisos.correo.viernes.cobros_detalle", idioma), chico = TRUE)))
  if (cxc && !is.null(d$vencidos_pct) && !is.na(d$vencidos_pct)) {
    delta <- d$vencidos_delta
    det <- if (!is.null(delta) && !is.na(delta)) paste0(
      '<b style="color:', if (delta > 0) .cr_C$cobre else .cr_C$verde, '">', .cr_flecha(delta), ' ',
      .cr_t("avisos.correo.viernes.vencidos_detalle", idioma, pts = .cr_pts(delta)), '</b>') else NULL
    m <- c(m, list(.cr_mosaico(.cr_t("avisos.correo.viernes.mosaico.vencidos", idioma),
                            .cr_e(sprintf("%.1f%%", d$vencidos_pct)), "borde", det, chico = TRUE)))
  }
  logros <- utils::head(as.character(d$logros %||% character()), CORREO_MAX_LOGROS)
  logros_html <- if (length(logros)) paste0(
    '<tr><td style="padding:20px 30px 0">', .cr_tabla(paste0(
      '<tr><td style="background:', .cr_C$logro, ';border:1px solid ', .cr_C$logro_borde, ';border-radius:14px;padding:16px 18px">',
      '<div style="font-size:11px;font-weight:900;letter-spacing:.18em;color:', .cr_C$logro_tinta, ';padding-bottom:8px">',
      .cr_t("avisos.correo.viernes.logros", idioma), '</div>',
      .cr_tabla(.cr_vinetas(as.list(.cr_e(logros)), "&#9733;", "14px"), estilo = "font-size:14px;line-height:1.5"),
      '</td></tr>')), '</td></tr>') else ""
  radar <- utils::head(as.character(d$radar %||% character()), CORREO_MAX_RADAR)
  radar_html <- if (length(radar)) paste0(
    .cr_titulo_seccion(.cr_t("avisos.correo.viernes.radar", idioma), "24px 30px 4px"),
    .cr_tabla(.cr_vinetas(as.list(.cr_e(radar))), estilo = "font-size:14px;line-height:1.5"), '</td></tr>') else ""
  cuerpo <- paste0(
    .cr_encabezado(.cr_e(.cr_mayus(correo_fecha(d$viernes, idioma))),
                list(marca = .cr_t("avisos.correo.marca", idioma), texto = .cr_t("avisos.correo.viernes.eyebrow", idioma)), titulo),
    .cr_rejilla(m, max(1L, length(m))), logros_html, radar_html,
    .cr_boton_y_pie(d$url %||% "", d$datos_sap, idioma))
  .cr_documento(correo_viernes_asunto(d, idioma), cuerpo)
}
