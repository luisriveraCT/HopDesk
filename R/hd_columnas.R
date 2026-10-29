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

col_texto <- function(campo, titulo = NULL, recorte = TRUE) {
  .hd_col("texto", campo, titulo, "izquierda", recorte = isTRUE(recorte),
    valores = function(x) {
      txt <- as.character(x)
      txt[is.na(txt)] <- ""
      list(mostrar = txt, orden = .hd_plano(txt), copiar = txt)
    })
}

col_dinero <- function(campo, titulo = NULL, moneda = "Moneda", sumar = TRUE) {
  .hd_col("dinero", campo, titulo, "derecha", recorte = FALSE,
    moneda = moneda, sumar = isTRUE(sumar),
    valores = function(x) {
      x   <- suppressWarnings(as.numeric(x))
      ok  <- !is.na(x)
      # sprintf + one regex for the thousands commas: formatC(big.mark =)
      # formats element by element and dominated the 5,000-row build.
      mostrar <- character(length(x))
      miles <- gsub("(\\d)(?=(\\d{3})+\\.)", "\\1,", sprintf("%.2f", abs(x[ok])), perl = TRUE)
      mostrar[ok] <- paste0(ifelse(x[ok] < 0, "-", ""), "$", miles)
      copiar <- character(length(x))
      copiar[ok] <- sub("\\.$", "", sub("0+$", "", sprintf("%.2f", x[ok])))
      list(mostrar = mostrar, orden = x, copiar = copiar)
    })
}

col_fecha <- function(campo, titulo = NULL) {
  .hd_col("fecha", campo, titulo, "izquierda", recorte = FALSE,
    valores = function(x) {
      d <- if (inherits(x, "Date")) x
           else if (inherits(x, "POSIXt")) as.Date(x)
           else suppressWarnings(as.Date(as.character(x), optional = TRUE))
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
    })
}

# mapa: named list, raw value -> list(texto = <key or text>, tono = <css tone>).
# Sorting follows the order the map declares; values not in the map go last
# and show as they come, in the neutral tone.
col_insignia <- function(campo, titulo = NULL, mapa = list()) {
  if (length(mapa) && (is.null(names(mapa)) || any(!nzchar(names(mapa)))))
    stop("col_insignia(): `mapa` must be a named list (raw value -> list(texto, tono)).",
         call. = FALSE)
  .hd_col("insignia", campo, titulo, "izquierda", recorte = FALSE, mapa = mapa,
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
    })
}

col_empresa <- function(campo = "Empresa", titulo = NULL) {
  col <- col_texto(campo, titulo, recorte = FALSE)
  col$tipo <- "empresa"
  col
}

col_etiqueta <- function(campo, titulo = NULL) {
  col <- col_texto(campo, titulo, recorte = TRUE)
  col$tipo <- "etiqueta"
  # Untagged rows sort after tagged ones in either direction of the text.
  base <- col$valores
  col$valores <- function(x) {
    v <- base(x)
    v$orden[!nzchar(v$orden)] <- NA_character_
    v
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
    sumar   = isTRUE(col$sumar)
  ))
}

# Rows, column by column:
#   list(ids, bloqueada, moneda, tipo,
#        celdas = list(<one per column>: list(mostrar, orden, copiar[, tono])))
# Control columns: .bloqueada (paid/confirmed: struck through, not
# selectable), .moneda and .tipo ("cobro"/"pago") for the totals footer.
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

  list(
    ids       = ids,
    bloqueada = bloqueada,
    moneda    = moneda,
    tipo      = tipo,
    celdas    = lapply(columnas, function(col) col$valores(datos[[col$campo]]))
  )
}
