# =============================================================================
# tests/helpers/flujo_adaptador.R
# What the Flujo de caja preview shows, in neutral terms (T17c), so one list
# of cases runs against today's preview (master: render_cashflow_preview_html)
# and, after the migration, against the engine's. Only this file knows how
# each one paints.
#
#   fl_rejilla(html, ed, day0 = list())
#     $columnas  the period labels in order (Día 0 and Total apart)
#     $bloques   one per currency, in order:
#       list(moneda, secciones = list(cobros = <sec>, pagos = <sec>), neto, acumulada)
#       <sec>     list(titulo, filas, subtotal)
#       a row     list(parte, provision, urgente, importante, visible,
#                      celdas = list(<k> = list(texto, valor)), total = list(texto, valor))
#       neto      list(titulo, celdas (texto, valor, tono), total)
#       acumulada list(titulo, dia0 = list(texto, valor), celdas, total)
# Texts are what the person reads ("$4.5M", "" for a 0); values are the exact
# numbers (from ed, what the export uses too), so a rounding change shows.
# Cells are keyed by position ("c1".."cN"): the labels move with the date.
# Tones: green "exito", red "peligro", grey "neutro" (today by colour).
# =============================================================================

.fl_tono <- function(estilo) {
  if (is.na(estilo) || !nzchar(estilo)) return("")
  if (grepl("#5dd87a", estilo, fixed = TRUE)) "exito" else if (grepl("#ff6b6b", estilo, fixed = TRUE)) "peligro"
  else if (grepl("#9ab5cc", estilo, fixed = TRUE)) "neutro" else ""
}

fl_rejilla <- function(html, ed, day0 = list()) {
  doc <- xml2::read_html(paste0("<html><body>", as.character(html), "</body></html>"), encoding = "UTF-8")
  labels <- ed$bucket_labels
  n <- length(labels)
  k <- paste0("c", seq_len(n))
  trs <- xml2::xml_find_all(doc, "//table[contains(@class,'cf-table')]/tbody/tr")
  ths <- xml2::xml_text(xml2::xml_find_all(doc, "//table[contains(@class,'cf-table')]/thead/tr/th"))
  txt <- function(td) trimws(xml2::xml_text(td))
  bloques <- list(); b <- NULL; sec <- NULL; ib <- 0L
  datos_de <- function(cur) ed$data[[match(cur, ed$currencies)]]
  for (tr in trs) {
    cls <- xml2::xml_attr(tr, "class") %||% ""
    tds <- xml2::xml_find_all(tr, "./td")
    nombre <- txt(tds[[1]])
    if (grepl("cf-cur-spacer", cls)) next
    if (grepl("cf-sec-hdr", cls)) {
      tipo <- if (grepl("cf-ar-hdr", cls)) "cobros" else "pagos"
      if (tipo == "cobros") {
        if (!is.null(b)) bloques[[length(bloques) + 1L]] <- b
        ib <- ib + 1L
        cur <- ed$currencies[ib]
        b <- list(moneda = cur, secciones = list())
      }
      sec <- tipo
      b$secciones[[tipo]] <- list(titulo = nombre, filas = list(), subtotal = NULL)
      next
    }
    dd <- datos_de(b$moneda)
    num_celdas <- tds[3:(n + 2L)]
    if (grepl("cf-parte-row", cls)) {
      wide <- if (sec == "cobros") dd$ar_wide else dd$ap_wide
      i <- length(b$secciones[[sec]]$filas) + 1L
      celdas <- setNames(lapply(seq_len(n), function(j)
        list(texto = txt(num_celdas[[j]]), valor = as.numeric(wide[[labels[j]]][i]))), k)
      b$secciones[[sec]]$filas[[i]] <- list(
        parte = trimws(xml2::xml_text(xml2::xml_find_first(tds[[1]], "./text()"))),
        provision = grepl("cf-prov-row", cls),
        urgente = !inherits(xml2::xml_find_first(tds[[1]], ".//span[@class='cf-tag-u']"), "xml_missing"),
        importante = !inherits(xml2::xml_find_first(tds[[1]], ".//span[@class='cf-tag-i']"), "xml_missing"),
        visible = !grepl("display:\\s*none", xml2::xml_attr(tr, "style") %||% ""),
        celdas = celdas,
        total = list(texto = txt(tds[[n + 3L]]), valor = as.numeric(wide$Total[i])))
    } else if (grepl("cf-subtot-row", cls)) {
      wide <- if (sec == "cobros") dd$ar_wide else dd$ap_wide
      vals <- vapply(labels, function(l) if (l %in% names(wide)) sum(as.numeric(wide[[l]]), na.rm = TRUE) else 0, numeric(1))
      b$secciones[[sec]]$subtotal <- list(titulo = nombre,
        celdas = setNames(lapply(seq_len(n), function(j) list(texto = txt(num_celdas[[j]]), valor = unname(vals[j]))), k),
        total = list(texto = txt(tds[[n + 3L]]), valor = sum(vals)))
    } else if (grepl("cf-net-row", cls)) {
      s <- dd$summary
      b$neto <- list(titulo = nombre,
        celdas = setNames(lapply(seq_len(n), function(j) list(texto = txt(num_celdas[[j]]), valor = s$net_day[j],
                                                              tono = .fl_tono(xml2::xml_attr(num_celdas[[j]], "style")))), k),
        total = list(texto = txt(tds[[n + 3L]]), valor = sum(s$net_day), tono = .fl_tono(xml2::xml_attr(tds[[n + 3L]], "style"))))
    } else if (grepl("cf-cum-row", cls)) {
      s <- dd$summary
      d0 <- as.numeric(day0[[b$moneda]] %||% 0)
      b$acumulada <- list(titulo = nombre,
        dia0 = list(texto = txt(tds[[2]]), valor = as.numeric(xml2::xml_attr(tds[[2]], "data-valor"))),
        celdas = setNames(lapply(seq_len(n), function(j) list(texto = txt(num_celdas[[j]]), valor = s$net_cum[j] + d0,
                                                              tono = .fl_tono(xml2::xml_attr(num_celdas[[j]], "style")))), k),
        total = list(texto = txt(tds[[n + 3L]]), valor = s$net_cum[n] + d0))
    }
  }
  if (!is.null(b)) bloques[[length(bloques) + 1L]] <- b
  list(encabezado = ths, columnas = labels, bloques = bloques)
}

fl_bloque <- function(r, moneda) { for (b in r$bloques) if (identical(b$moneda, moneda)) return(b); NULL }
fl_fila <- function(r, moneda, seccion, parte) {
  b <- fl_bloque(r, moneda)
  for (f in b$secciones[[seccion]]$filas) if (identical(f$parte, parte)) return(f)
  NULL
}
