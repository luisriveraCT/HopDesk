# ── hd_table columns: typed constructors and the data sent to the browser ────
# Each col_*() returns a column spec: tipo, campo, titulo, alinear, recorte,
# and valores(x), which turns the column's raw vector into the three values
# every cell carries:
#   mostrar  text already formatted for people, in the active language
#            ("$1,234.50", "29 oct 2026"); painted with textContent, so it is
#            plain text, never HTML
#   orden    what sorting compares: a number, or ISO / normalized text
#   copiar   the raw value the copy button puts on the clipboard
#            ("1234.5", "2026-10-29")
#
# `titulo` may be a dictionary key ("tablas.columna.importe") or plain text.
#
# hd_filas() builds the payload for the browser column by column (one vector
# per column, not one object per row) so 5,000 rows stay small and fast to
# build in R.

# Optional columns (columnas_opc): opcional = TRUE puts the column in the
# table's "Columnas" menu, and visible says whether it starts shown. Every
# col_*() takes both, last, so existing calls don't change.
.hd_opcional <- function(col, opcional, visible) {
  if (!is.logical(opcional) || length(opcional) != 1L || is.na(opcional) ||
      !is.logical(visible) || length(visible) != 1L || is.na(visible))
    stop(sprintf("col_%s(\"%s\"): `opcional` and `visible` must be TRUE or FALSE.", col$tipo, col$campo), call. = FALSE)
  if (!opcional && !visible)
    stop(sprintf("col_%s(\"%s\"): only an optional column can start hidden (opcional = TRUE).", col$tipo, col$campo),
         call. = FALSE)
  col$opcional <- opcional
  col$visible  <- visible
  col
}

# fija (T17a, piece `columna_fija`): the column stays at the left while the
# table scrolls sideways (the name of a matrix of periods: Flujo de caja,
# Pasivos). Only the first columns, one after another, and never an
# optional one (hd_table_server checks it).
.hd_fija <- function(col, fija) {
  if (!is.logical(fija) || length(fija) != 1L || is.na(fija))
    stop(sprintf("col_%s(\"%s\"): `fija` must be TRUE or FALSE.", col$tipo, col$campo), call. = FALSE)
  if (fija) col$fija <- TRUE
  col
}

# The tones a cell can take (tono_celda): the badges' own (www/hdtable.css).
HD_TONOS_CELDA <- c("exito", "aviso", "peligro", "info", "neutro", "morado")

# accion_celda (T17b): the marks a cell can carry before its amount, and the
# buttons it can carry after it (each sends its name as the action). Icons
# are escapes, like HD_ACCIONES_FILA's.
HD_CELDA_MARCAS <- list(
  pendiente  = list(icono = "\u25f7", clave = "tablas.celda.pendiente"),
  vencida    = list(icono = "!",      clave = "tablas.celda.vencida"),
  confirmada = list(icono = "\u2713", clave = "tablas.celda.confirmada"))
HD_CELDA_BOTONES <- list(
  provision   = list(icono = "\u26a1", clave = "tablas.accion.provision"),
  comprobante = list(icono = "\u21a9", clave = "tablas.accion.comprobante"))

# Dictionary key -> text; anything that isn't a key is used as-is.
.hd_texto <- function(x) {
  if (is.character(x) && length(x) == 1L && !is.null(i18n_dict("es")[[x]])) tr(x) else x
}

# Lowercase, accent-free: what search and text sorting compare.
.hd_plano <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  stringi::stri_trans_general(tolower(x), "Latin-ASCII")
}

.hd_col <- function(tipo, campo, titulo, alinear, valores, ...) {
  if (!is.character(campo) || length(campo) != 1L || !nzchar(campo))
    stop(sprintf("col_%s(): `campo` must be one column name.", tipo), call. = FALSE)
  structure(list(tipo = tipo, campo = campo, titulo = titulo %||% campo,
                 alinear = alinear, valores = valores, ...),
            class = "hd_columna")
}

# ancho: the column's width in px, for short values that would otherwise
# take a share of the spare width like any other column (Documento): the
# rest of the value is cut, and the bubble shows it whole.
col_texto <- function(campo, titulo = NULL, recorte = TRUE, opcional = FALSE, visible = TRUE, ancho = NULL,
                      fija = FALSE) {
  if (!is.null(ancho) && !(is.numeric(ancho) && length(ancho) == 1L && !is.na(ancho) && ancho > 0))
    stop(sprintf("col_texto(\"%s\"): `ancho` must be one positive number (px).", campo), call. = FALSE)
  .hd_fija(.hd_opcional(.hd_col("texto", campo, titulo, "izquierda", recorte = isTRUE(recorte), ancho = ancho,
    valores = function(x) {
      txt <- as.character(x)
      txt[is.na(txt)] <- ""
      list(mostrar = txt, orden = .hd_plano(txt), copiar = txt)
    }), opcional, visible), fija)
}

# editable (T14, piece `edicion_celda`): an amount per row typed in the cell
# (Abono parcial). list(min = 0, max = "Saldo", aviso = "tablas.abono.mayor_saldo"):
# min / max are a number or the name of a field of the row (each row its
# own bound); aviso is the dictionary key shown when a value falls out of
# range ({min} and {max} come formatted; default tablas.edicion.mayor /
# .menor). The column's own value is where the edit starts. A table with an
# editable column must have the piece, and that column must be the one it
# sums (hd_table_server checks both).
#
# A matrix of periods (T17a; Flujo de caja, Pasivos):
#   formato = "compacto"  (piece dinero_compacto) "$4.5M", "$802K", "$950";
#                         the bubble, Copiar and the Excel keep the full amount
#   cero = "vacio"        (dinero_compacto) a 0 paints nothing (it still
#                         copies and exports 0)
#   tono = "signo"        (piece tono_celda) green above 0, red below, grey at 0
#   tono = "<campo>"      (tono_celda) the tone each row's field names, one of
#                         HD_TONOS_CELDA (anything else: no tone); a state, so
#                         it tints the cell too
#   destacar = TRUE       (tono_celda) the whole column shaded (today's)
#   fija = TRUE           (columna_fija) see .hd_fija()
#   accion = TRUE         (piece accion_celda, T17b) each cell is something of
#                         its own (a provision of a liability on a date). The
#                         row's fields <campo>.id (what a click on the cell
#                         sends; empty: the cell doesn't respond), <campo>.marca
#                         (HD_CELDA_MARCAS: pendiente ◷ / vencida ! /
#                         confirmada ✓), <campo>.boton (HD_CELDA_BOTONES: ⚡ /
#                         ↩, in the cell), <campo>.n (how many things the
#                         cell adds up: above 1 it says so) and <campo>.mano
#                         (TRUE: changed by hand, a dot in its corner). Any
#                         missing: none.
col_dinero <- function(campo, titulo = NULL, moneda = "Moneda", sumar = TRUE, opcional = FALSE, visible = TRUE,
                       editable = NULL, fija = FALSE, formato = "completo", cero = "cifra", tono = NULL,
                       destacar = FALSE, accion = FALSE) {
  donde <- sprintf("col_dinero(\"%s\")", campo)
  if (!(is.character(formato) && length(formato) == 1L && formato %in% c("completo", "compacto")))
    stop(donde, ": `formato` must be \"completo\" or \"compacto\".", call. = FALSE)
  if (!(is.character(cero) && length(cero) == 1L && cero %in% c("cifra", "vacio")))
    stop(donde, ": `cero` must be \"cifra\" or \"vacio\".", call. = FALSE)
  if (!is.null(tono) && !(is.character(tono) && length(tono) == 1L && !is.na(tono) && nzchar(tono)))
    stop(donde, ": `tono` must be \"signo\" or the name of a field of the row.", call. = FALSE)
  if (!is.logical(destacar) || length(destacar) != 1L || is.na(destacar))
    stop(donde, ": `destacar` must be TRUE or FALSE.", call. = FALSE)
  col <- .hd_fija(.hd_opcional(.hd_col("dinero", campo, titulo, "derecha", recorte = FALSE,
    moneda = moneda, sumar = isTRUE(sumar), editable = .hd_editable_validar(editable, campo),
    valores = function(x) {
      # Round first: -0.001 must show "$0.00" and copy "0", not "-$0.00" / "-0"
      # (and sprintf prints a negative zero as "-0.00", hence the reset).
      x   <- round(suppressWarnings(as.numeric(x)), 2)
      ok  <- !is.na(x)
      x[ok & x == 0] <- 0
      # sprintf + one regex for the thousands commas: formatC(big.mark =)
      # formats element by element and dominated the 5,000-row build.
      mostrar <- character(length(x))
      miles <- gsub("(\\d)(?=(\\d{3})+\\.)", "\\1,", sprintf("%.2f", abs(x[ok])), perl = TRUE)
      mostrar[ok] <- paste0(ifelse(x[ok] < 0, "-", ""), "$", miles)
      copiar <- character(length(x))
      copiar[ok] <- sub("\\.$", "", sub("0+$", "", sprintf("%.2f", x[ok])))
      v <- list(mostrar = mostrar, orden = x, copiar = copiar)
      if (formato == "compacto") {
        v$completo <- mostrar
        v$mostrar  <- .hd_dinero_compacto(x)
      }
      if (cero == "vacio") {
        v$mostrar[ok & x == 0] <- ""
        if (!is.null(v$completo)) v$completo[ok & x == 0] <- ""
      }
      if (identical(tono, "signo"))
        v$tono <- ifelse(!ok, "", ifelse(x > 0, "exito", ifelse(x < 0, "peligro", "neutro")))
      v
    }), opcional, visible), fija)
  # Only what a column asks for travels: no other table's message changes.
  if (formato != "completo") col$formato <- formato
  if (cero != "cifra") col$cero <- cero
  if (!is.null(tono)) col$tono <- tono
  if (destacar) col$destacar <- TRUE
  if (!is.logical(accion) || length(accion) != 1L || is.na(accion))
    stop(donde, ": `accion` must be TRUE or FALSE.", call. = FALSE)
  if (accion) col$accion <- TRUE
  col
}

# "$4.5M" / "$802K" / "$950" (dinero_compacto): one decimal in millions, none
# below. The unit is chosen by what the amount rounds to, so 999,600 is
# "$1.0M", not "$1000K", and 999.6 is "$1K", not "$1,000". Halves round up
# (2,500 is "$3K"): sprintf alone rounds them to even, and 1,500 and 2,500
# both read "$2K". One dictionary lookup per unit, filled in vectorized (a
# matrix has tens of thousands of cells).
.hd_dinero_compacto <- function(x) {
  out <- character(length(x))
  ok  <- !is.na(x)
  ax  <- abs(x[ok])
  m   <- ax >= 999500
  k   <- !m & ax >= 999.5
  u   <- !m & !k
  arriba <- function(v) floor(v + 0.5 + 1e-9)   # 1e-9: 4,450,000 / 1e5 is 44.4999...
  txt <- character(length(ax))
  txt[m] <- stringi::stri_replace_all_fixed(tr("tablas.dinero.millones"), "{n}", sprintf("%.1f", arriba(ax[m] / 1e5) / 10))
  txt[k] <- stringi::stri_replace_all_fixed(tr("tablas.dinero.miles"), "{n}", sprintf("%.0f", arriba(ax[k] / 1e3)))
  txt[u] <- gsub("(\\d)(?=(\\d{3})+$)", "\\1,", sprintf("%.0f", arriba(ax[u])), perl = TRUE)
  out[ok] <- paste0(ifelse(x[ok] < 0, "-", ""), "$", txt)
  out
}

.hd_editable_validar <- function(editable, campo) {
  if (is.null(editable)) return(NULL)
  donde <- sprintf("col_dinero(\"%s\"): `editable`", campo)
  if (!is.list(editable))
    stop(donde, " must be list(min = , max = , aviso = ).", call. = FALSE)
  desconocidos <- setdiff(names(editable), c("min", "max", "paso", "aviso"))
  if (length(desconocidos) || (length(editable) && is.null(names(editable))))
    stop(sprintf("%s: unknown field(s): %s.", donde, paste(desconocidos, collapse = ", ")), call. = FALSE)
  for (k in c("min", "max")) {
    v <- editable[[k]]
    if (!is.null(v) && !((is.numeric(v) || is.character(v)) && length(v) == 1L && !is.na(v) &&
                         (is.numeric(v) || nzchar(v))))
      stop(sprintf("%s$%s must be one number or the name of a field of the row.", donde, k), call. = FALSE)
  }
  if (is.numeric(editable$min) && is.numeric(editable$max) && editable$min > editable$max)
    stop(donde, ": min is greater than max.", call. = FALSE)
  paso <- editable$paso %||% 0.01
  if (!is.numeric(paso) || length(paso) != 1L || is.na(paso) || paso <= 0)
    stop(donde, "$paso must be a positive number.", call. = FALSE)
  if (!is.null(editable$aviso) && !(is.character(editable$aviso) && length(editable$aviso) == 1L))
    stop(donde, "$aviso must be one dictionary key.", call. = FALSE)
  list(min = editable$min, max = editable$max, paso = paso, aviso = editable$aviso)
}

# A row's bounds for an editable column: a number for every row, or the
# values of the field it names (NA = no bound).
.hd_editable_limite <- function(lim, datos) {
  n <- nrow(datos)
  if (is.null(lim)) return(rep(NA_real_, n))
  if (is.numeric(lim)) return(rep(as.numeric(lim), n))
  suppressWarnings(as.numeric(datos[[lim]]))
}

# A plain number (rows, ms, quantities): right-aligned, thousands commas,
# `decimales` places, sorted by value (as text "9" went after "10") and
# exported as a number. No currency and never summed in the footer.
col_numero <- function(campo, titulo = NULL, decimales = 0L, opcional = FALSE, visible = TRUE) {
  if (!is.numeric(decimales) || length(decimales) != 1L || is.na(decimales) || decimales < 0)
    stop(sprintf("col_numero(\"%s\"): `decimales` must be one number >= 0.", campo), call. = FALSE)
  decimales <- as.integer(decimales)
  .hd_opcional(.hd_col("numero", campo, titulo, "derecha", recorte = FALSE, decimales = decimales,
    valores = function(x) {
      x  <- round(suppressWarnings(as.numeric(x)), decimales)
      ok <- !is.na(x)
      x[ok & x == 0] <- 0
      f  <- sprintf(paste0("%.", decimales, "f"), abs(x[ok]))
      miles <- gsub(if (decimales) "(\\d)(?=(\\d{3})+\\.)" else "(\\d)(?=(\\d{3})+$)", "\\1,", f, perl = TRUE)
      mostrar <- character(length(x))
      mostrar[ok] <- paste0(ifelse(x[ok] < 0, "-", ""), miles)
      copiar <- character(length(x))
      copiar[ok] <- sprintf(paste0("%.", decimales, "f"), x[ok])
      list(mostrar = mostrar, orden = x, copiar = copiar)
    }), opcional, visible)
}

# Date, POSIXct or ISO text -> Date (NA where it isn't one).
.hd_como_fecha <- function(x) {
  if (inherits(x, "Date")) x
  # The value's own time zone: as.Date() would use UTC and turn 20:00 in
  # Mexico City into the next day.
  else if (inherits(x, "POSIXt")) as.Date(format(x, "%Y-%m-%d"))
  else suppressWarnings(as.Date(as.character(x), optional = TRUE))
}

col_fecha <- function(campo, titulo = NULL, opcional = FALSE, visible = TRUE) {
  .hd_opcional(.hd_col("fecha", campo, titulo, "izquierda", recorte = FALSE,
    valores = function(x) {
      d <- .hd_como_fecha(x)
      ok  <- !is.na(d)
      iso <- character(length(d))
      iso[ok] <- format(d[ok], "%Y-%m-%d")
      mostrar <- character(length(d))
      if (any(ok)) {
        # One template lookup, filled in vectorized (a tr() per row is slow
        # at 5,000 rows). The template orders day/month/year per language.
        meses <- vapply(1:12, function(m) tr(paste0("comun.mes.", m)), character(1))
        lt <- as.POSIXlt(d[ok])
        txt <- rep(tr("comun.fecha_corta"), sum(ok))
        txt <- stringi::stri_replace_all_fixed(txt, "{dia}",  as.character(lt$mday))
        txt <- stringi::stri_replace_all_fixed(txt, "{mes}",  meses[lt$mon + 1L])
        txt <- stringi::stri_replace_all_fixed(txt, "{anio}", as.character(lt$year + 1900L))
        mostrar[ok] <- txt
      }
      orden <- iso
      orden[!ok] <- NA_character_
      list(mostrar = mostrar, orden = orden, copiar = iso)
    }), opcional, visible)
}

# Date and time (the registers: Actividad, Archivos de pago, pronósticos).
# Shows the date like col_fecha plus "hh:mm" (segundos = TRUE: "hh:mm:ss"),
# in the value's own time zone, and sorts by the full instant -- text in
# dd/mm/aaaa sorted wrong. POSIXct or ISO text ("2026-11-02 08:40:54").
col_fecha_hora <- function(campo, titulo = NULL, segundos = FALSE, opcional = FALSE, visible = TRUE) {
  fecha <- col_fecha(campo)$valores
  .hd_opcional(.hd_col("fecha_hora", campo, titulo, "izquierda", recorte = FALSE,
    valores = function(x) {
      # Each value with the first format it reads in (as.POSIXct's tryFormats
      # needs ALL of them in one format, so a single bad value blanked all).
      t <- if (inherits(x, "POSIXt")) as.POSIXct(x) else {
        s <- as.character(x)
        t <- as.POSIXct(rep(NA_real_, length(s)), origin = "1970-01-01")
        for (f in c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M")) {
          falta <- is.na(t) & !is.na(s)
          if (any(falta)) t[falta] <- as.POSIXct(strptime(s[falta], f, tz = ""))
        }
        t
      }
      ok  <- !is.na(t)
      iso <- character(length(t))
      iso[ok] <- format(t[ok], "%Y-%m-%d %H:%M:%S")
      mostrar <- character(length(t))
      if (any(ok)) {
        d <- fecha(format(t[ok], "%Y-%m-%d"))$mostrar
        mostrar[ok] <- paste(d, format(t[ok], if (isTRUE(segundos)) "%H:%M:%S" else "%H:%M"))
      }
      orden <- iso
      orden[!ok] <- NA_character_
      list(mostrar = mostrar, orden = orden, copiar = iso)
    }), opcional, visible)
}

# mapa: named list, raw value -> list(texto = <key or text>, tono = <css tone>).
# Sorting follows the order the map declares; values not in the map go last
# and show as they come, in the neutral tone.
col_insignia <- function(campo, titulo = NULL, mapa = list(), opcional = FALSE, visible = TRUE) {
  if (length(mapa) && (is.null(names(mapa)) || any(!nzchar(names(mapa)))))
    stop("col_insignia(): `mapa` must be a named list (raw value -> list(texto, tono)).",
         call. = FALSE)
  .hd_opcional(.hd_col("insignia", campo, titulo, "izquierda", recorte = FALSE, mapa = mapa,
    valores = function(x) {
      raw <- as.character(x)
      raw[is.na(raw)] <- ""
      pos <- match(raw, names(mapa))
      mostrar <- ifelse(is.na(pos), raw, vapply(raw, function(v)
        if (is.null(mapa[[v]])) v else .hd_texto(mapa[[v]]$texto %||% v), character(1)))
      tono <- vapply(raw, function(v) mapa[[v]]$tono %||% "neutro", character(1),
                     USE.NAMES = FALSE)
      list(mostrar = unname(mostrar), orden = ifelse(is.na(pos), length(mapa) + 1L, pos),
           copiar = raw, tono = tono)
    }), opcional, visible)
}

# iniciales: the company map (initials = name, shared$company_map()). A value
# that is a company's full name shows as its initials, and the bubble shows
# the full name (`completo`); sorting, copying and filters keep the value as
# it came. The day modal gets names ("Networks Crossdocking Services") that
# made its table scroll sideways (T09).
col_empresa <- function(campo = "Empresa", titulo = NULL, opcional = FALSE, visible = TRUE, iniciales = NULL,
                        fija = FALSE) {
  col <- col_texto(campo, titulo, recorte = FALSE, opcional = opcional, visible = visible, fija = fija)
  col$tipo <- "empresa"
  if (length(iniciales)) {
    texto <- col$valores
    col$valores <- function(x) {
      v <- texto(x)
      ini <- names(iniciales)[match(v$copiar, unname(iniciales))]
      v$mostrar <- ifelse(is.na(ini), v$mostrar, ini)
      v$completo <- ifelse(is.na(ini), NA_character_, v$copiar)
      v
    }
  }
  col
}

# opcion_fila: one choice per row, a one-line segmented control
# ("[ Nuevo | Mantener | Otro ]"; the conflicts of a Pasivos edit, T18).
#   opciones    c(<value> = "<key or text>"), 2 to 4, short
#   al_escribir the option that typing in the table's editable column
#               (edicion_celda) picks; with any other, what was typed doesn't
#               count (t$ediciones() says valido = FALSE)
#   burbuja     c(<value> = "<field>"): that option's button shows, on hover,
#               the row's value of that column ("Nuevo": the suggested amount)
# The data's value is where each row starts; after that the choice is the
# person's, by id (t$opciones(), t$poner_opcion()). One per table (checked
# by hd_table_server, like the piece).
col_opcion <- function(campo, titulo = NULL, opciones, al_escribir = NULL, burbuja = NULL,
                       opcional = FALSE, visible = TRUE) {
  donde <- sprintf("col_opcion(\"%s\")", campo)
  if (!is.character(opciones) || length(opciones) < 2L || length(opciones) > 4L || is.null(names(opciones)) ||
      any(!nzchar(names(opciones))) || anyDuplicated(names(opciones)) || any(!nzchar(opciones)))
    stop(donde, ": `opciones` must be c(<value> = \"<key or text>\"), 2 to 4 of them.", call. = FALSE)
  if (!is.null(al_escribir) && !(is.character(al_escribir) && length(al_escribir) == 1L && al_escribir %in% names(opciones)))
    stop(donde, ": `al_escribir` must be one of its opciones.", call. = FALSE)
  if (!is.null(burbuja) && (!is.character(burbuja) || is.null(names(burbuja)) || !all(names(burbuja) %in% names(opciones))))
    stop(donde, ": `burbuja` must be c(<option> = \"<field>\").", call. = FALSE)
  col <- .hd_opcional(.hd_col("opcion", campo, titulo, "izquierda", recorte = FALSE,
    opciones = opciones, al_escribir = al_escribir, burbuja = burbuja,
    valores = function(x) {
      raw <- as.character(x)
      malas <- unique(raw[is.na(raw) | !raw %in% names(opciones)])
      if (length(malas))
        stop(sprintf("%s: value(s) %s not among its opciones (%s).", donde,
                     paste(ifelse(is.na(malas), "NA", malas), collapse = ", "), paste(names(opciones), collapse = ", ")),
             call. = FALSE)
      texto <- vapply(opciones[raw], .hd_texto, character(1), USE.NAMES = FALSE)
      list(mostrar = texto, orden = match(raw, names(opciones)), copiar = texto, valor = raw)
    }), opcional, visible)
  col
}

# ── Tags (etiquetas) ────────────────────────────────────────────────────────
# The one map for invoice tags: from it come the label, the pill's colour,
# the row's background (by tone, in www/hdtable.css) and the order. The
# stored values never change (urgent / both / important); Luis renamed what
# people see to A, B and C so the name doesn't mean anything by itself.
# tinta / pastilla / fondo: the same colours as the tone in hdtable.css, for
# what is painted outside the engine (the Calendario and its DT tables).
HD_ETIQUETAS <- list(
  urgent    = list(texto = "tablas.etiquetas.a", tono = "rojo",     peso = 1L,
                   tinta = "#b42318", pastilla = "#ffe1de", fondo = "#fff0f0"),
  both      = list(texto = "tablas.etiquetas.b", tono = "naranja",  peso = 2L,
                   tinta = "#b54708", pastilla = "#ffe8d1", fondo = "#fff3e6"),
  important = list(texto = "tablas.etiquetas.c", tono = "amarillo", peso = 3L,
                   tinta = "#8a6100", pastilla = "#fff1b8", fondo = "#fffbe6")
)

# Outside the engine. The tag of an invoice from its stored tags (urgent
# and important together are "both"), "" without one.
hd_etiqueta_clave <- function(tags) {
  hi <- "important" %in% tags
  ur <- "urgent"    %in% tags
  if (hi && ur) "both" else if (ur) "urgent" else if (hi) "important" else ""
}

# A tag's pill ("A" on red, ...), from its key or its label; NULL without one.
hd_etiqueta_pastilla <- function(clave, class = "badge ms-1", style = "font-size:0.65rem;") {
  e <- HD_ETIQUETAS[[clave]] %||% Find(function(x) identical(tr(x$texto), clave), HD_ETIQUETAS)
  if (is.null(e)) return(NULL)
  htmltools::tags$span(class = class, style = sprintf("%scolor:%s;background:%s;", style, e$tinta, e$pastilla),
                       tr(e$texto))
}

# A DT table's row background by its tag label column (DT::formatStyle):
# DT::styleEqual(hd_etiquetas_dt$etiquetas, hd_etiquetas_dt$fondos).
hd_etiquetas_dt <- function() {
  list(etiquetas = c(vapply(HD_ETIQUETAS, function(e) tr(e$texto), character(1), USE.NAMES = FALSE), ""),
       fondos    = c(vapply(HD_ETIQUETAS, `[[`, character(1), "fondo", USE.NAMES = FALSE), "transparent"))
}

# A tag column holds the stored values. Known ones show their label as a
# pill (with the `etiquetas` piece) and sort A, B, C; any other text shows
# as it comes, after them; untagged rows go last in either direction.
col_etiqueta <- function(campo, titulo = NULL, opcional = FALSE, visible = TRUE) {
  mapa <- lapply(HD_ETIQUETAS, function(e) list(texto = e$texto, tono = e$tono))
  col <- col_texto(campo, titulo, recorte = TRUE, opcional = opcional, visible = visible)
  col$tipo <- "etiqueta"
  col$mapa <- mapa
  col$valores <- function(x) {
    raw <- as.character(x)
    raw[is.na(raw)] <- ""
    conocida <- raw %in% names(HD_ETIQUETAS)
    mostrar <- raw
    mostrar[conocida] <- vapply(raw[conocida], function(v) .hd_texto(HD_ETIQUETAS[[v]]$texto), character(1))
    orden <- ifelse(conocida, paste0("0", vapply(raw, function(v) if (v %in% names(HD_ETIQUETAS)) HD_ETIQUETAS[[v]]$peso else 0L, integer(1))),
                    paste0("1", .hd_plano(raw)))
    orden[!nzchar(raw)] <- NA_character_
    tono <- ifelse(conocida, vapply(raw, function(v) if (v %in% names(HD_ETIQUETAS)) HD_ETIQUETAS[[v]]$tono else "", character(1)), "neutro")
    list(mostrar = unname(mostrar), orden = unname(orden), copiar = unname(mostrar), tono = unname(tono))
  }
  col
}

# ── Payload for the browser ─────────────────────────────────────────────────
hd_validar_columnas <- function(columnas, datos) {
  if (!length(columnas) || !all(vapply(columnas, inherits, logical(1), "hd_columna")))
    stop("hd_table: `columnas` must be a non-empty list of col_*() specs.", call. = FALSE)
  faltan <- setdiff(vapply(columnas, `[[`, character(1), "campo"), names(datos))
  if (length(faltan))
    stop(sprintf("hd_table: column(s) not in the data: %s.", paste(faltan, collapse = ", ")),
         call. = FALSE)
  # An editable column's bounds may name fields of the row (max = "Saldo").
  for (col in columnas) for (k in c("min", "max")) {
    lim <- col$editable[[k]]
    if (is.character(lim) && !lim %in% names(datos))
      stop(sprintf("hd_table: col_dinero(\"%s\") editable$%s names '%s', which is not in the data.",
                   col$campo, k, lim), call. = FALSE)
  }
  # tono = "<campo>": each row's tone comes from that field (tono_celda).
  for (col in columnas)
    if (!is.null(col$tono) && !identical(col$tono, "signo") && !col$tono %in% names(datos))
      stop(sprintf("hd_table: col_dinero(\"%s\") tono names '%s', which is not in the data.",
                   col$campo, col$tono), call. = FALSE)
  invisible(TRUE)
}

hd_validar_ids <- function(datos, id_col) {
  if (!id_col %in% names(datos))
    stop(sprintf("hd_table: id column '%s' is not in the data.", id_col), call. = FALSE)
  ids <- as.character(datos[[id_col]])
  if (anyNA(ids) || any(!nzchar(ids)))
    stop(sprintf("hd_table: id column '%s' has %d empty value(s); every row needs an id.",
                 id_col, sum(is.na(ids) | !nzchar(ids))), call. = FALSE)
  rep_ids <- unique(ids[duplicated(ids)])
  if (length(rep_ids))
    stop(sprintf("hd_table: id column '%s' repeats: %s.", id_col,
                 paste(utils::head(rep_ids, 5), collapse = ", ")), call. = FALSE)
  ids
}

# Column metadata (no data) for the browser.
hd_columnas_meta <- function(columnas) {
  lapply(columnas, function(col) {
   m <- list(
    campo   = col$campo,
    titulo  = .hd_texto(col$titulo),
    tipo    = col$tipo,
    alinear = col$alinear,
    recorte = isTRUE(col$recorte),
    sumar   = isTRUE(col$sumar),
    opcional = isTRUE(col$opcional),
    visible  = !identical(col$visible, FALSE),
    ancho    = if (is.null(col$ancho)) NA else col$ancho
   )
   # Only an editable column carries it, so no other table's message changes
   # (the per-row bounds go with the rows).
   if (!is.null(col$editable)) {
     propio <- col$editable$aviso
     m$editable <- list(paso  = col$editable$paso,
                        mayor = .hd_texto(propio %||% "tablas.edicion.mayor"),
                        menor = .hd_texto(propio %||% "tablas.edicion.menor"))
   }
   # Likewise (T17a): only a fixed or shaded column says so.
   if (isTRUE(col$fija)) m$fija <- TRUE
   if (isTRUE(col$accion)) m$accion <- TRUE
   if (isTRUE(col$destacar)) m$destacar <- TRUE
   # The sign colours the text; a state (a field) tints the cell too.
   if (!is.null(col$tono)) m$tono <- if (identical(col$tono, "signo")) "signo" else "estado"
   # opcion_fila: the options in order, and what typing / the bubble tie to.
   if (identical(col$tipo, "opcion")) {
     m$opciones <- lapply(names(col$opciones), function(v) {
       o <- list(valor = v, texto = .hd_texto(col$opciones[[v]]))
       if (!is.null(col$burbuja) && !is.na(col$burbuja[v])) o$burbuja <- col$burbuja[[v]]
       o
     })
     if (!is.null(col$al_escribir)) m$al_escribir <- col$al_escribir
   }
   m
  })
}

# Rows, column by column:
#   list(ids, bloqueada, moneda, tipo,
#        celdas = list(<one per column>: list(mostrar, orden, copiar[, tono])))
# Control columns: .bloqueada (paid/confirmed: not selectable; struck
# through when the table has the `tachado` piece), .moneda and .tipo
# ("cobro"/"pago") for the totals footer, .provision (logical: purple
# provision row with its badge), .acciones (row actions allowed per row),
# .fijar (logical: always shown whatever the month, on top as a block in any
# order; it still follows the search, chips and filters), .agregado
# (logical: a check, "already in Agenda de Hoy"; it doesn't lock the row) and
# .adjuntos (integer: a clip with the count that sends accion "adjuntos").
# Without .moneda, the currency comes from the first summed money column's
# `moneda` field, then from "Moneda"; without .tipo, totals aren't split.
hd_filas <- function(datos, columnas, id_col = "id") {
  datos <- as.data.frame(datos, stringsAsFactors = FALSE)
  hd_validar_columnas(columnas, datos)
  ids <- hd_validar_ids(datos, id_col)
  n   <- nrow(datos)

  bloqueada <- if (".bloqueada" %in% names(datos)) as.logical(datos[[".bloqueada"]]) else rep(FALSE, n)
  bloqueada[is.na(bloqueada)] <- FALSE

  col_suma   <- Filter(function(c) identical(c$tipo, "dinero") && isTRUE(c$sumar), columnas)
  campo_mon  <- if (length(col_suma)) col_suma[[1]]$moneda else "Moneda"
  moneda <- if (".moneda" %in% names(datos)) as.character(datos[[".moneda"]])
            else if (!is.null(campo_mon) && campo_mon %in% names(datos)) as.character(datos[[campo_mon]])
            else rep(NA_character_, n)
  tipo <- if (".tipo" %in% names(datos)) as.character(datos[[".tipo"]]) else rep(NA_character_, n)
  tipo[!tipo %in% c("cobro", "pago")] <- NA_character_
  provision <- if (".provision" %in% names(datos)) as.logical(datos[[".provision"]]) else rep(FALSE, n)
  provision[is.na(provision)] <- FALSE
  fijar <- if (".fijar" %in% names(datos)) as.logical(datos[[".fijar"]]) else rep(FALSE, n)
  fijar[is.na(fijar)] <- FALSE
  agregado <- if (".agregado" %in% names(datos)) as.logical(datos[[".agregado"]]) else rep(FALSE, n)
  agregado[is.na(agregado)] <- FALSE
  adjuntos <- if (".adjuntos" %in% names(datos)) suppressWarnings(as.integer(datos[[".adjuntos"]])) else rep(0L, n)
  adjuntos[is.na(adjuntos) | adjuntos < 0L] <- 0L
  # .acciones: the row actions each row may show, as a list column of names
  # or comma-separated text. Without the column every row shows them all; a
  # row with NA or nothing shows none (fails closed).
  acciones <- if (".acciones" %in% names(datos)) {
    a <- datos[[".acciones"]]
    vapply(seq_len(n), function(i) {
      v <- if (is.list(a)) unlist(a[[i]]) else a[i]
      v <- trimws(unlist(strsplit(as.character(v %||% character(0)), ",", fixed = TRUE)))
      paste(v[!is.na(v) & nzchar(v)], collapse = ",")
    }, character(1))
  } else NULL

  list(
    # I(): Shiny serializes with auto_unbox = TRUE, so without it a 1-row
    # table would send "a1" instead of ["a1"] for every per-row vector.
    ids       = I(ids),
    bloqueada = I(bloqueada),
    moneda    = I(moneda),
    tipo      = I(tipo),
    provision = I(provision),
    fijar     = I(fijar),
    agregado  = I(agregado),
    adjuntos  = I(adjuntos),
    celdas   = lapply(columnas, function(col) {
      v <- lapply(col$valores(datos[[col$campo]]), I)
      # An editable column: each row's bounds (edicion_celda, T14).
      if (!is.null(col$editable)) {
        v$min <- I(.hd_editable_limite(col$editable$min, datos))
        v$max <- I(.hd_editable_limite(col$editable$max, datos))
      }
      # accion_celda (T17b): what each cell is, from <campo>.id / .marca /
      # .boton / .n; only the closed lists, anything else is nothing.
      if (isTRUE(col$accion)) {
        campo_de <- function(sufijo, vacio) {
          x <- datos[[paste0(col$campo, sufijo)]]
          if (is.null(x)) rep(vacio, n) else x
        }
        cid <- as.character(campo_de(".id", "")); cid[is.na(cid)] <- ""
        ma  <- as.character(campo_de(".marca", "")); ma[is.na(ma) | !ma %in% names(HD_CELDA_MARCAS)] <- ""
        bo  <- as.character(campo_de(".boton", "")); bo[is.na(bo) | !bo %in% names(HD_CELDA_BOTONES)] <- ""
        nn  <- suppressWarnings(as.integer(campo_de(".n", 0L))); nn[is.na(nn) | nn < 0L] <- 0L
        mano <- as.logical(campo_de(".mano", FALSE)); mano[is.na(mano)] <- FALSE
        v$cid <- I(cid); v$marca <- I(ma); v$boton <- I(bo); v$n <- I(nn); v$mano <- I(mano)
      }
      # tono = "<campo>" (tono_celda, T17a): the row's own tone, only from
      # the closed list; anything else paints none.
      if (!is.null(col$tono) && !identical(col$tono, "signo")) {
        tn <- as.character(datos[[col$tono]])
        tn[is.na(tn) | !tn %in% HD_TONOS_CELDA] <- ""
        v$tono <- I(tn)
      }
      v
    })
  ) -> out
  # Only when the column exists: list(x = NULL) would serialize as {}.
  if (!is.null(acciones)) out$acciones <- I(acciones)
  # semaforo (T17b): .semaforo, how urgent the row is -- "vencida",
  # "muy_pronto", "pronto" or nothing: a bar and a tint (Pasivos: overdue,
  # due within 2 days, within 7).
  if (".semaforo" %in% names(datos)) {
    s <- as.character(datos[[".semaforo"]])
    s[is.na(s) | !s %in% c("vencida", "muy_pronto", "pronto")] <- ""
    out$semaforo <- I(s)
  }
  # sangria (T17b): .nivel, 1 = a child of the row above it (a liability's
  # fee), shown indented with "↳"; 0 or nothing = a row of its own.
  if (".nivel" %in% names(datos)) {
    nv <- suppressWarnings(as.integer(datos[[".nivel"]]))
    nv[is.na(nv) | nv < 0L] <- 0L
    out$nivel <- I(pmin(nv, 1L))
  }
  out
}
