# =============================================================================
# tests/helpers/pasivos_tabla_adaptador.R
# What Pasivos · Tabla shows, in neutral terms, so one list of cases
# (tests/test_pasivos_tabla_caracterizacion_*.R) runs against today's table
# (master) and, after T17b, against the engine's. Only this file knows how
# each one paints.
#
#   pt_rejilla(html, hoy, granularidad)   # master: the table's HTML
#     $columnas  one per period: list(clave = "d-3" | "s+1" | "m+0" | "a+0", hoy)
#     $filas     in order: list(tipo = "moneda", moneda) or
#                list(tipo = "fila", etiqueta, parte, moneda, comision, urgencia,
#                     editar = <liability id the ✎ sends> | NA, celdas = <named by clave>)
#     a cell:    list(texto, marca = "pendiente"|"vencido"|"confirmado"|"",
#                     boton = "convertir"|"comprobante"|"", boton_id, clic_id,
#                     estado = "provision"|"provision_vencida"|"pendiente"|
#                              "pendiente_vencido"|"confirmado", a_mano, vence_hoy)
#   pt_buscar_fila(rejilla, etiqueta)     # a row by its label
#
# Periods are keyed by their distance from today (the table reads Sys.Date()),
# so the reference JSON holds on any day.
# =============================================================================

.pt_clave_periodo <- function(lbl, hoy, granularidad) {
  hoy <- as.Date(hoy)
  if (granularidad == "day") return(sprintf("d%+d", as.integer(as.Date(lbl) - hoy)))
  dias <- seq(hoy - 2000L, hoy + 2000L, by = "day")
  fmt  <- switch(granularidad, week = "%G-W%V", month = "%Y-%m", year = "%Y")
  pref <- switch(granularidad, week = "s", month = "m", year = "a")
  et   <- unique(format(dias, fmt))
  sprintf("%s%+d", pref, match(lbl, et) - match(format(hoy, fmt), et))
}

# The id inside onclick="Shiny.setInputValue('<input>','<id>',...)".
.pt_id_onclick <- function(onclick, input) {
  if (is.na(onclick) || !nzchar(onclick)) return(NA_character_)
  m <- regmatches(onclick, regexec(sprintf("setInputValue\\('%s','(.*?)',\\{", input), onclick))[[1]]
  if (length(m) < 2L) NA_character_ else m[2]
}

pt_rejilla <- function(html, hoy, granularidad = "day") {
  doc <- xml2::read_html(paste0("<html><body>", as.character(html), "</body></html>"), encoding = "UTF-8")
  ths <- xml2::xml_find_all(doc, "//table[contains(@class,'pasivos-table')]/thead/tr/th")
  if (!length(ths)) return(list(columnas = list(), filas = list(), vacio = trimws(xml2::xml_text(doc))))
  et  <- xml2::xml_text(ths)[-1]
  claves <- vapply(et, .pt_clave_periodo, character(1), hoy = hoy, granularidad = granularidad, USE.NAMES = FALSE)
  columnas <- unname(Map(function(k, th) list(clave = k, hoy = grepl("pasivos-table-today-col", xml2::xml_attr(th, "class"))),
                  claves, ths[-1]))
  tiene <- function(cls, x) grepl(paste0("(^|\\s)", x, "(\\s|$)"), cls)
  filas <- lapply(xml2::xml_find_all(doc, "//table[contains(@class,'pasivos-table')]/tbody/tr"), function(tr) {
    cls <- xml2::xml_attr(tr, "class") %||% ""
    tds <- xml2::xml_find_all(tr, "./td")
    if (tiene(cls, "pasivos-currency-group-row"))
      return(list(tipo = "moneda", moneda = trimws(xml2::xml_text(tds[[1]]))))
    eti <- tds[[1]]
    comision <- tiene(cls, "pasivos-fee-row")
    nodo_txt <- function(xp) { n <- xml2::xml_find_first(eti, xp); if (inherits(n, "xml_missing")) "" else xml2::xml_text(n) }
    lapiz <- xml2::xml_find_first(eti, ".//button[contains(@class,'pasivos-row-edit-btn')]")
    celdas <- lapply(tds[-1], function(td) {
      c2 <- xml2::xml_attr(td, "class") %||% ""
      btn <- xml2::xml_find_first(td, ".//button[contains(@class,'pasivos-convert-btn')]")
      div <- xml2::xml_find_first(td, "./div[@onclick]")
      spans <- xml2::xml_text(xml2::xml_find_all(td, ".//span"))
      marca <- if ("◷" %in% spans) "pendiente" else if ("!" %in% spans) "vencido" else if ("✓" %in% spans) "confirmado" else ""
      boton <- if (inherits(btn, "xml_missing")) "" else if (grepl("⚡", xml2::xml_text(btn))) "convertir" else "comprobante"
      texto <- trimws(gsub("^!\\s*", "", trimws(gsub("[◷✓⚡↩]", "", xml2::xml_text(td)))))
      estado <- if (tiene(c2, "pasivos-cell-item-confirmed")) "confirmado"
        else if (tiene(c2, "pasivos-cell-overdue-red")) "pendiente_vencido"
        else if (tiene(c2, "pasivos-cell-item-pending")) "pendiente"
        else if (tiene(c2, "pasivos-cell-overdue-amber")) "provision_vencida"
        else if (tiene(c2, "pasivos-cell-provision")) "provision" else ""
      list(texto = texto, marca = marca, boton = boton,
           boton_id = if (nzchar(boton)) .pt_id_onclick(xml2::xml_attr(btn, "onclick"), "pasivos_convert_request") else NA_character_,
           clic_id = if (inherits(div, "xml_missing")) NA_character_ else .pt_id_onclick(xml2::xml_attr(div, "onclick"), "pasivos_cell_click"),
           estado = estado, a_mano = tiene(c2, "pasivos-cell-has-override"), vence_hoy = tiene(c2, "pasivos-cell-due-today"))
    })
    names(celdas) <- claves
    celdas <- Filter(function(x) nzchar(x$texto) || nzchar(x$estado), celdas)
    list(tipo = "fila",
         etiqueta = if (comision) nodo_txt(".//span[contains(@class,'text-truncate')]") else nodo_txt(".//span[contains(@class,'fw-semibold')]"),
         parte = if (comision) "" else nodo_txt(".//span[contains(@class,'text-muted')]"),
         moneda = if (comision) "" else nodo_txt(".//span[contains(@class,'pasivos-cur-pill')]"),
         comision = comision,
         urgencia = if (tiene(cls, "has-overdue")) "vencida" else if (tiene(cls, "has-very-soon")) "muy_pronto"
                    else if (tiene(cls, "has-due-soon")) "pronto" else "",
         editar = if (inherits(lapiz, "xml_missing")) NA_character_ else .pt_id_onclick(xml2::xml_attr(lapiz, "onclick"), "pasivos_edit_liability"),
         # Whether the ✎ would really send it: master writes the id inside a
         # single-quoted JS string (C26); the engine sends ids, not code.
         editar_funciona = if (inherits(lapiz, "xml_missing")) NA else
           grepl("^Shiny\\.setInputValue\\('pasivos_edit_liability','([^'\\\\]|\\\\.)*',\\{", xml2::xml_attr(lapiz, "onclick")),
         celdas = celdas)
  })
  list(columnas = columnas, filas = filas)
}

pt_buscar_fila <- function(rejilla, etiqueta) {
  for (f in rejilla$filas) if (identical(f$tipo, "fila") && identical(f$etiqueta, etiqueta)) return(f)
  NULL
}
pt_etiquetas <- function(rejilla) vapply(Filter(function(f) f$tipo == "fila", rejilla$filas), `[[`, character(1), "etiqueta")
