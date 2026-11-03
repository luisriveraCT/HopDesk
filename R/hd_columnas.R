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
col_texto <- function(campo, titulo = NULL, recorte = TRUE, opcional = FALSE, visible = TRUE, ancho = NULL) {
  if (!is.null(ancho) && !(is.numeric(ancho) && length(ancho) == 1L && !is.na(ancho) && ancho > 0))
    stop(sprintf("col_texto(\"%s\"): `ancho` must be one positive number (px).", campo), call. = FALSE)
  .hd_opcional(.hd_col("texto", campo, titulo, "izquierda", recorte = isTRUE(recorte), ancho = ancho,
    valores = function(x) {
      txt <- as.character(x)
      txt[is.na(txt)] <- ""
      list(mostrar = txt, orden = .hd_plano(txt), copiar = txt)
    }), opcional, visible)
}

col_dinero <- function(campo, titulo = NULL, moneda = "Moneda", sumar = TRUE, opcional = FALSE, visible = TRUE) {
  .hd_opcional(.hd_col("dinero", campo, titulo, "derecha", recorte = FALSE,
    moneda = moneda, sumar = isTRUE(sumar),
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
      list(mostrar = mostrar, orden = x, copiar = copiar)
    }), opcional, visible)
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
col_empresa <- function(campo = "Empresa", titulo = NULL, opcional = FALSE, visible = TRUE, iniciales = NULL) {
  col <- col_texto(campo, titulo, recorte = FALSE, opcional = opcional, visible = visible)
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
  lapply(columnas, function(col) list(
    campo   = col$campo,
    titulo  = .hd_texto(col$titulo),
    tipo    = col$tipo,
    alinear = col$alinear,
    recorte = isTRUE(col$recorte),
    sumar   = isTRUE(col$sumar),
    opcional = isTRUE(col$opcional),
    visible  = !identical(col$visible, FALSE),
    ancho    = if (is.null(col$ancho)) NA else col$ancho
  ))
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
    celdas   = lapply(columnas, function(col) lapply(col$valores(datos[[col$campo]]), I))
  ) -> out
  # Only when the column exists: list(x = NULL) would serialize as {}.
  if (!is.null(acciones)) out$acciones <- I(acciones)
  out
}
