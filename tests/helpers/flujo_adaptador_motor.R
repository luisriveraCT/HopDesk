# =============================================================================
# tests/helpers/flujo_adaptador_motor.R
# The same neutral grid as tests/helpers/flujo_adaptador.R (fl_rejilla), read
# from what the engine gets after T17c: cashflow_tabla_datos() and
# cashflow_tabla_columnas() through hd_filas(), the payload the browser
# paints. A group's row (its sums) is worked out as the browser's
# sumaColumna() does it: compact, empty at 0; the Total in whole pesos.
#
#   fl_rejilla_motor(ed, day0 = list(), expandido = FALSE)
# Same shape as fl_rejilla(); where the engine says it differently the
# texts are the engine's (the [CAMBIA] are in the tests, each one named).
# A section's titulo is its group's ("Cobros"); its subtotal's titulo too.
# =============================================================================

fl_rejilla_motor <- function(ed, day0 = list(), expandido = FALSE) {
  d    <- cashflow_tabla_datos(ed, day0)
  cols <- cashflow_tabla_columnas(ed$bucket_labels)
  f    <- hd_filas(d, cols)
  campos <- vapply(cols, `[[`, character(1), "campo")
  cel <- function(campo) f$celdas[[match(campo, campos)]]
  txt <- function(campo, i) unclass(cel(campo)$mostrar)[i]
  val <- function(campo, i) unclass(cel(campo)$orden)[i]
  ton <- function(campo, i) { t <- unclass(cel(campo)$tono); if (is.null(t)) "" else t[i] }
  n <- length(ed$bucket_labels); k <- paste0("c", seq_len(n)); b <- paste0("b", seq_len(n))
  compacto_vacio <- function(v) if (abs(v) < 0.005) "" else .hd_dinero_compacto(v)
  entero_vacio <- function(v) if (abs(v) < 0.005) "" else .hd_dinero_entero(v)
  bloques <- list()
  for (cur in ed$currencies) {
    blq <- list(moneda = cur, secciones = list())
    for (lado in c("ar", "ap")) {
      i <- which(d$Moneda == cur & startsWith(d$id, paste0(cur, "|", lado, "|")))
      if (!length(i)) next
      sec <- if (lado == "ar") "cobros" else "pagos"
      filas <- lapply(i, function(r) list(
        parte = d$Contraparte[r], provision = isTRUE(d$.provision[r]),
        urgente = d$Etiqueta[r] %in% c("urgent", "both"), importante = d$Etiqueta[r] %in% c("important", "both"),
        visible = expandido,
        celdas = setNames(lapply(seq_len(n), function(j) list(texto = txt(b[j], r), valor = val(b[j], r))), k),
        total = list(texto = txt("Total", r), valor = val("Total", r))))
      sumas <- vapply(b, function(cb) sum(val(cb, i), na.rm = TRUE), numeric(1))
      st <- sum(val("Total", i), na.rm = TRUE)
      blq$secciones[[sec]] <- list(titulo = d$Tipo[i[1]], filas = filas,
        subtotal = list(titulo = d$Tipo[i[1]],
          celdas = setNames(lapply(seq_len(n), function(j) list(texto = compacto_vacio(sumas[[j]]), valor = unname(sumas[[j]]))), k),
          total = list(texto = entero_vacio(st), valor = st)))
    }
    r <- which(d$id == paste0("neto|", cur))
    if (length(r))
      blq$neto <- list(titulo = d$Contraparte[r],
        celdas = setNames(lapply(seq_len(n), function(j) list(texto = txt(b[j], r), valor = val(b[j], r), tono = ton(b[j], r))), k),
        total = list(texto = txt("Total", r), valor = val("Total", r), tono = ton("Total", r)))
    r <- which(d$id == paste0("acum|", cur))
    if (length(r))
      blq$acumulada <- list(titulo = d$Contraparte[r],
        dia0 = list(texto = txt("Dia0", r), valor = val("Dia0", r)),
        celdas = setNames(lapply(seq_len(n), function(j) list(texto = txt(b[j], r), valor = val(b[j], r), tono = ton(b[j], r))), k),
        total = list(texto = txt("Total", r), valor = val("Total", r)))
    bloques[[length(bloques) + 1L]] <- blq
  }
  list(encabezado = vapply(cols, `[[`, character(1), "titulo"), columnas = ed$bucket_labels, bloques = bloques,
       datos = d, filas = f)
}
