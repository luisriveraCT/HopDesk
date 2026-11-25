# =============================================================================
# tests/test_t110_excel_insignias.R
# T110: a badge column (col_insignia) exports to Excel the text the user sees,
# in their language -- never its raw value ("active", "por_pagar"). Found by
# E3 in T109: hd_excel_escribir() wrote v$copiar (the raw value) for every
# table with badges.
#   1  two real tables: Lista de Pasivos (its Categoría) and Requisiciones
#      (its Etapa), in es and in en; the other columns as before
#   2  guardian, the engine: for ANY badge column the cell is
#      col$valores()$mostrar -- a value the map doesn't know stays as it is
#   3  guardian, the maps: every dictionary key a col_insignia() of R/ gives
#      as a badge's texto exists in es and en (else the "text" exported is the
#      key itself)
# Run from project root:  Rscript tests/test_t110_excel_insignias.R
# =============================================================================
cat("=== test_t110_excel_insignias ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = character()) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else { cat(" FAIL:", label, "\n"); for (d in detalle) cat("       ", d, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R"))
  suppressMessages(source(f))
tomar <- function(archivo, nombres)
  for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, globalenv())
tomar("R/pasivos_list_module.R", ".pasivos_col_categoria")
tomar("R/requisiciones_module.R", c(".reqm_col_prioridad", ".reqm_mapa_etapas", ".reqm_columnas"))

excel <- function(datos, columnas) {
  f <- tempfile(fileext = ".xlsx")
  hd_excel_escribir(datos, columnas, ids = datos$id, id_col = "id", archivo = f)
  openxlsx::read.xlsx(f, sheet = 1, check.names = FALSE, sep.names = " ")
}
en_idioma <- function(idioma, expr) { viejo <- options(hopdesk.idioma = idioma); on.exit(options(viejo)); force(expr) }

# ── 1 · Two real tables ──────────────────────────────────────────────────────
pas <- data.frame(id = c("L1", "L2", "L3"), nombre = c("Renta", "Crédito", "Visa"),
                  categoria = c("regular", "financiero", "tarjeta"), stringsAsFactors = FALSE)
cols_p <- list(col_texto("nombre", "tablas.pasivos.nombre"), .pasivos_col_categoria())
for (idioma in c("es", "en")) {
  x <- en_idioma(idioma, excel(pas, cols_p))
  esperado <- en_idioma(idioma, vapply(c("regular", "financiero", "tarjeta"),
                                       function(k) tr(paste0("tablas.pasivos.cat.", k), idioma = idioma), ""))
  ok(sprintf("1 · Lista de Pasivos (%s): la Categoría sale con su texto (%s), no el valor crudo", idioma,
             paste(esperado, collapse = " / ")),
     identical(unname(x[[2]]), unname(esperado)) && !any(x[[2]] %in% pas$categoria))
  ok(sprintf("1 · Lista de Pasivos (%s): el Nombre, igual que antes", idioma), identical(x[[1]], pas$nombre))
}
req <- data.frame(id = c("R1", "R2", "R3"), folio = c("RQ-1", "RQ-2", "RQ-3"), titulo = "T", prioridad_txt = "8",
                  carpeta_ruta = "", proveedor = "P", empresa = "NCS", monto_mostrado = 10, moneda_mostrada = "MXN",
                  etapa = c("por_pagar", "en_agenda", "pagada"), stringsAsFactors = FALSE)
cols_r <- Filter(function(c) c$campo %in% c("folio", "etapa"), .reqm_columnas(iniciales = NULL))
for (idioma in c("es", "en")) {
  x <- en_idioma(idioma, excel(req, cols_r))
  esperado <- vapply(req$etapa, function(k) tr(paste0("requisiciones.etapa.", k), idioma = idioma), "")
  ok(sprintf("1 · Requisiciones (%s): la Etapa sale con su texto (%s), nunca \"por_pagar\"", idioma,
             paste(esperado, collapse = " / ")),
     identical(unname(x[[2]]), unname(esperado)) && !any(x[[2]] %in% req$etapa))
}

# ── 2 · Guardian: the engine ─────────────────────────────────────────────────
col_x <- col_insignia("e", "tablas.pasivos.estado", mapa = list(
  active = list(texto = "tablas.pasivos.activo", tono = "exito"), paused = list(texto = "Pausa propia", tono = "aviso")))
d <- data.frame(id = c("a", "b", "c", "d"), e = c("active", "paused", "otro", NA), stringsAsFactors = FALSE)
for (idioma in c("es", "en")) {
  x <- en_idioma(idioma, excel(d, list(col_x)))
  v <- en_idioma(idioma, col_x$valores(d$e))$mostrar
  ok(sprintf("2 · el motor (%s): cada insignia exporta lo que ve el usuario (valores()$mostrar)", idioma),
     identical(ifelse(is.na(x[[1]]), "", x[[1]]), v))
}
x <- excel(d, list(col_x))
ok("2 · un valor que el mapa no conoce sale tal cual; vacío sigue vacío", identical(x[[1]][3], "otro") && x[[1]][4] %in% c("", NA))
col_f <- col_insignia("e", "tablas.pasivos.estado", mapa = list(active = list(texto = "=HYPERLINK(1)", tono = "exito")))
ok("2 · el texto mostrado también se neutraliza (una fórmula nunca llega como fórmula)",
   !startsWith(excel(d[1, ], list(col_f))[[1]], "="))

# ── 3 · Guardian: every badge's text exists in es and en ─────────────────────
def <- list()
for (f in list.files("R", pattern = "[.]R$", recursive = TRUE, full.names = TRUE))
  for (e in tryCatch(parse(f, keep.source = FALSE, encoding = "UTF-8"), error = function(e) expression()))
    if (is.call(e) && as.character(e[[1]])[1] %in% c("<-", "=") && is.name(e[[2]]) && is.null(def[[as.character(e[[2]])]]))
      def[[as.character(e[[2]])]] <- e[[3]]
textos <- function(x, prof = 0L) {
  out <- character()
  if (prof > 3L) return(out)
  if (is.call(x)) {
    a <- as.list(x)
    if (!is.null(names(a)) && "texto" %in% names(a) && is.character(a$texto)) out <- c(out, a$texto)
    if (is.name(a[[1]]) && !is.null(def[[as.character(a[[1]])]]))       # a map from a function: .reqm_mapa_etapas()
      out <- c(out, textos(def[[as.character(a[[1]])]], prof + 1L))
    for (k in a[-1]) out <- c(out, textos(k, prof))
  } else if (is.name(x) && !is.null(def[[as.character(x)]]) && prof < 3L) {   # a map in a constant: PROV_ESTADOS
    out <- c(out, textos(def[[as.character(x)]], prof + 1L))
  } else if (is.function(x) || is.expression(x)) for (k in as.list(x)) out <- c(out, textos(k, prof))
  out
}
claves <- character(); n_ins <- 0L
for (f in list.files("R", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)) {
  andar <- function(x) {
    if (is.call(x)) {
      if (identical(x[[1]], as.name("col_insignia"))) {
        n_ins <<- n_ins + 1L
        a <- as.list(x)
        m <- a$mapa %||% (if (length(a) >= 4L && is.null(names(a)[4]) || identical(names(a)[4], "")) a[[4]])
        if (!is.null(m)) claves <<- c(claves, textos(m))
      }
      for (k in as.list(x)[-1]) if (!missing(k)) andar(k)
    }
  }
  for (e in tryCatch(parse(f, keep.source = FALSE, encoding = "UTF-8"), error = function(e) expression())) andar(e)
}
claves <- unique(claves[grepl("^[a-z_]+([.][a-z0-9_]+)+$", claves)])
ok(sprintf("3 · el recorrido ve las insignias de R/ (%d llamadas, %d claves)", n_ins, length(claves)),
   n_ins >= 40L && length(claves) >= 60L && "tablas.pasivos.activo" %in% claves && "requisiciones.etapa.por_pagar" %in% claves)
faltan <- claves[!(claves %in% names(i18n_dict("es")) & claves %in% names(i18n_dict("en")))]
ok("3 · cada texto de insignia existe en es y en: ninguna se exporta con su clave cruda", !length(faltan),
   sprintf("%s: add it to www/i18n/es.json and en.json (the badge would export the key itself)", faltan))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
