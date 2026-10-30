# ── hd_table: one table engine for HopDesk ───────────────────────────────────
# The server formats the rows once (R/hd_columnas.R) and sends them to the
# browser; www/hdtable.js paints, sorts, searches, selects and sums there.
# The browser only ever sends back ids and actions -- nothing by position.
#
#   t <- hd_table_server("facturas", tabla = "vencidos", datos = reactive(df),
#                        columnas = list(col_empresa("Empresa"), ...),
#                        piezas = c("densidad"), id_col = "id")
#   t$seleccion()   # ids marked, in the order they're shown
#   t$accion()      # list(accion, ids, nonce) or NULL
#   t$periodo()     # "AAAA-MM" shown (with `periodo`), else NULL
#
# Server -> browser: one "hdt-datos" custom message each time `datos`
# changes. Browser -> server: ns("sel") and ns("accion"), priority "event".
#
# accion() values: "detalle" (double click on a row, locked rows too),
# "encabezado" (the header switch, with `encabezado`; ids "si" or "no"), and
# the names given in acciones_fila (one row's id) and acciones_barra. Actions
# that confirm ("eliminar" on a row, bar actions with confirmar = TRUE) only
# reach accion() once the user presses Confirmar in the table's own bar.
#
# With the `detalle` piece, seleccion() changes twice on a double click: the
# first click marks the row (and sends it), then the double click undoes it
# (and sends it again). Code observing seleccion() sees both.
#
# `columnas` may be a list of col_*() or a reactive returning one (a table
# pasted from Excel changes columns with each paste and with the header
# switch). When the columns change, the table keeps the sort (if its column's
# field is still there), the search and the selection.
#
# PERMISSIONS: the .acciones control column only HIDES row buttons the user
# may not use. A hidden button is not a permission check -- the module that
# uses the table must check the permission again when the action arrives.

# Relative paths (like vencidos.js), so the app also works under a sub-path.
hd_table_deps <- function() {
  htmltools::singleton(htmltools::tags$head(
    i18n_head(),
    htmltools::tags$link(rel = "stylesheet", href = "hdtable.css?v=13"),
    htmltools::tags$script(src = "hdtable.js?v=14")
  ))
}

# alto: height of the scrolling area (the totals footer sits below it).
#   "auto" (default): as tall as its rows, never fewer than ~8; up to ~15
#   compact rows while 30 or fewer are visible, and above that it grows to
#   70% of the window (inside a modal, what fits without page scroll). No
#   table ever cuts, pages or hides rows.
#   A CSS length ("420px", "60vh") is used as the maximum height, as before.
hd_table_ui <- function(id, alto = "auto") {
  ns <- shiny::NS(id)
  auto <- is.null(alto) || identical(alto, "auto")
  htmltools::tagList(
    hd_table_deps(),
    htmltools::div(
      id = ns("hdt"), class = paste("hdt", if (auto) "hdt-auto"), `data-ns` = ns(""),
      style = if (!auto) paste0("--hdt-alto:", alto, ";"),
      htmltools::div(class = "hdt-cargando", tr("comun.cargando"))
    )
  )
}

# An optional argument whose piece the table doesn't have is ignored, with
# one message per table and argument (like pieces not built yet).
.hd_sin_pieza <- function(tabla, arg, pieza, valor, piezas) {
  if (is.null(valor) || pieza %in% piezas) return(valor)
  clave <- paste(tabla, arg, sep = "|")
  if (is.null(.hd_avisadas[[clave]])) {
    .hd_avisadas[[clave]] <- TRUE
    message(sprintf("hd_table '%s': `%s` ignored, the table doesn't have the '%s' piece.", tabla, arg, pieza))
  }
  NULL
}

# ── fichas ──────────────────────────────────────────────────────────────────
# fichas = list(campo = "etapa", orden = c(...), mostrar = "conteo" | "monto")
# One "Todas" chip plus one per value: first the values in `orden`, then any
# other value in the data, in order of appearance.
hd_fichas_validar <- function(fichas) {
  if (is.null(fichas)) return(NULL)
  if (!is.list(fichas) || !is.character(fichas$campo) || length(fichas$campo) != 1L || !nzchar(fichas$campo))
    stop("hd_table: `fichas` must be list(campo = <column name>, orden = <values>, mostrar = \"conteo\" | \"monto\").",
         call. = FALSE)
  desconocidos <- setdiff(names(fichas), c("campo", "orden", "mostrar"))
  if (length(desconocidos))
    stop(sprintf("hd_table: unknown `fichas` field(s): %s.", paste(desconocidos, collapse = ", ")), call. = FALSE)
  fichas$mostrar <- fichas$mostrar %||% "conteo"
  if (!fichas$mostrar %in% c("conteo", "monto"))
    stop(sprintf("hd_table: fichas$mostrar must be \"conteo\" or \"monto\", not \"%s\".", fichas$mostrar), call. = FALSE)
  fichas$orden <- as.character(fichas$orden %||% character(0))
  fichas
}

hd_fichas_payload <- function(fichas, datos, columnas) {
  if (is.null(fichas)) return(NULL)
  if (!fichas$campo %in% names(datos))
    stop(sprintf("hd_table: fichas$campo '%s' is not in the data.", fichas$campo), call. = FALSE)
  if (identical(fichas$mostrar, "monto") &&
      !any(vapply(columnas, function(c) identical(c$tipo, "dinero") && isTRUE(c$sumar), logical(1))))
    stop("hd_table: fichas$mostrar = \"monto\" needs a col_dinero() with sumar = TRUE.", call. = FALSE)
  valores <- as.character(datos[[fichas$campo]])
  valores[is.na(valores)] <- ""
  presentes <- unique(valores[nzchar(valores)])
  orden <- c(fichas$orden, setdiff(presentes, fichas$orden))
  # Labels from the col_insignia() of the same field, if there is one.
  ins <- Filter(function(c) identical(c$tipo, "insignia") && identical(c$campo, fichas$campo), columnas)
  mapa <- if (length(ins)) ins[[1]]$mapa else list()
  list(
    valores  = I(valores),
    opciones = lapply(orden, function(v) list(valor = v, texto = .hd_texto(mapa[[v]]$texto %||% v))),
    mostrar  = fichas$mostrar
  )
}

# ── filtros ─────────────────────────────────────────────────────────────────
# filtros = list(list(campo = "Moneda"), list(campo = "Empresa", titulo = <key or text>), ...)
# One button per filter in the bar ("Moneda: Todas"), with a checkbox menu of
# the values in the data and their counts. Values chosen in one filter
# combine with OR; filters, with AND; all of it with the search and the chips.
hd_filtros_validar <- function(filtros) {
  if (is.null(filtros)) return(NULL)
  if (!is.list(filtros) || !length(filtros) || !all(vapply(filtros, is.list, logical(1))))
    stop("hd_table: `filtros` must be a list of list(campo = <column name>, titulo = <key or text>).",
         call. = FALSE)
  out <- lapply(seq_along(filtros), function(k) {
    f <- filtros[[k]]
    if (!is.character(f$campo) || length(f$campo) != 1L || !nzchar(f$campo))
      stop(sprintf("hd_table: filtros[[%d]] needs `campo` (one column name).", k), call. = FALSE)
    otros <- setdiff(names(f), c("campo", "titulo"))
    if (length(otros))
      stop(sprintf("hd_table: filtro '%s': unknown field(s) %s.", f$campo, paste(otros, collapse = ", ")),
           call. = FALSE)
    if (!is.null(f$titulo) && (!is.character(f$titulo) || length(f$titulo) != 1L || !nzchar(f$titulo)))
      stop(sprintf("hd_table: filtro '%s': `titulo` must be a dictionary key or text.", f$campo), call. = FALSE)
    list(campo = f$campo, titulo = f$titulo)
  })
  campos <- vapply(out, `[[`, character(1), "campo")
  if (anyDuplicated(campos))
    stop(sprintf("hd_table: filtros repeats '%s'.", campos[duplicated(campos)][1]), call. = FALSE)
  out
}

# Options: the values in the data, in the order of the field's col_insignia()
# map (labels from it too, like the chips), then the rest alphabetically,
# and "(Sin valor)" last when some row has none.
hd_filtros_payload <- function(filtros, datos, columnas) {
  if (is.null(filtros)) return(NULL)
  lapply(filtros, function(f) {
    if (!f$campo %in% names(datos))
      stop(sprintf("hd_table: filtro '%s' is not a column in the data.", f$campo), call. = FALSE)
    valores <- as.character(datos[[f$campo]])
    valores[is.na(valores)] <- ""
    col  <- Find(function(c) identical(c$campo, f$campo), columnas)
    mapa <- if (!is.null(col) && identical(col$tipo, "insignia")) col$mapa else list()
    presentes <- unique(valores[nzchar(valores)])
    resto <- setdiff(presentes, names(mapa))
    orden <- c(intersect(names(mapa), presentes), resto[order(.hd_plano(resto), resto)])
    opciones <- lapply(orden, function(v) list(valor = v, texto = .hd_texto(mapa[[v]]$texto %||% v)))
    if (!all(nzchar(valores)))
      opciones <- c(opciones, list(list(valor = "", texto = tr("tablas.filtros.vacio"))))
    list(campo = f$campo, titulo = .hd_texto(f$titulo %||% col$titulo %||% f$campo),
         valores = I(valores), opciones = opciones)
  })
}

# ── periodo (nav_periodo) ───────────────────────────────────────────────────
# periodo = list(campo = "fecha", inicial = Sys.Date())
# "‹ oct 2026 ›" in the bar shows one month of that date column; a click on
# the month opens a month/year picker. t$periodo() is the month shown,
# "AAAA-MM", for a server that only wants to load that month. Rows with
# .fijar = TRUE show whatever the month (rows with no date, only then).
.hd_mes <- function(x) {
  if (is.character(x) && length(x) == 1L && grepl("^[0-9]{4}-[0-9]{2}$", x)) x <- paste0(x, "-01")
  d <- .hd_como_fecha(x)
  if (length(d) != 1L || is.na(d)) return(NA_character_)
  format(d, "%Y-%m")
}

hd_periodo_validar <- function(periodo, columnas) {
  if (is.null(periodo)) return(NULL)
  if (!is.list(periodo) || !is.character(periodo$campo) || length(periodo$campo) != 1L || !nzchar(periodo$campo))
    stop("hd_table: `periodo` must be list(campo = <date column>, inicial = <date or \"AAAA-MM\">).", call. = FALSE)
  otros <- setdiff(names(periodo), c("campo", "inicial"))
  if (length(otros))
    stop(sprintf("hd_table: unknown `periodo` field(s): %s.", paste(otros, collapse = ", ")), call. = FALSE)
  col <- Find(function(c) identical(c$campo, periodo$campo), columnas)
  if (!is.null(col) && !identical(col$tipo, "fecha"))
    stop(sprintf("hd_table: periodo$campo '%s' is shown as a %s column, not a col_fecha().",
                 periodo$campo, col$tipo), call. = FALSE)
  inicial <- .hd_mes(periodo$inicial %||% Sys.Date())
  if (is.na(inicial))
    stop("hd_table: periodo$inicial must be a date or \"AAAA-MM\".", call. = FALSE)
  list(campo = periodo$campo, inicial = inicial)
}

# Each row's month ("AAAA-MM", "" without a date). The field must hold dates:
# Date, POSIXct or ISO text; anything else that doesn't read as one stops.
hd_periodo_payload <- function(periodo, datos) {
  if (is.null(periodo)) return(NULL)
  if (!periodo$campo %in% names(datos))
    stop(sprintf("hd_table: periodo$campo '%s' is not in the data.", periodo$campo), call. = FALSE)
  x <- datos[[periodo$campo]]
  d <- .hd_como_fecha(x)
  texto <- if (inherits(x, c("Date", "POSIXt"))) character(0) else trimws(as.character(x))
  if (length(texto) && any(is.na(d) & !is.na(texto) & nzchar(texto)))
    stop(sprintf("hd_table: periodo$campo '%s' is not a date column (e.g. \"%s\").", periodo$campo,
                 texto[is.na(d) & !is.na(texto) & nzchar(texto)][1]), call. = FALSE)
  meses <- ifelse(is.na(d), "", format(d, "%Y-%m"))
  list(meses = I(meses), inicial = periodo$inicial)
}

# ── Row actions (acc_fila) ──────────────────────────────────────────────────
# Fixed catalog: icon, dictionary key for the title, and the order the
# buttons always appear in, right-aligned. "provision" only shows on rows
# with .provision = TRUE.
HD_ACCIONES_FILA <- list(
  crear_provision = list(icono = "",       clave = "tablas.accion.crear_provision", estilo = "texto"),
  agregar         = list(icono = "\uff0b", clave = "tablas.accion.agregar",   estilo = "icono"),
  editar          = list(icono = "\u270e", clave = "tablas.accion.editar",    estilo = "icono"),
  provision       = list(icono = "\u26a1", clave = "tablas.accion.provision", estilo = "icono"),
  deshacer        = list(icono = "\u21b6", clave = "tablas.accion.deshacer",  estilo = "icono"),
  detalle         = list(icono = "\u2139", clave = "tablas.accion.detalle",   estilo = "icono"),
  eliminar        = list(icono = "\u2715", clave = "tablas.accion.eliminar",  estilo = "icono",
                         confirmar = TRUE, peligro = TRUE)
)
# crear_provision is the only button with text: it opens the provision form
# (it is neither "provision" = convert to payment, nor "agregar" = send to
# Agenda). eliminar always goes through the confirmation bar, even for one
# row (Luis: deletes and bulk actions confirm).

hd_acciones_fila_validar <- function(acciones_fila) {
  if (is.null(acciones_fila)) return(NULL)
  if (!is.character(acciones_fila) || !length(acciones_fila))
    stop("hd_table: `acciones_fila` must be a character vector of action names.", call. = FALSE)
  desconocidas <- setdiff(acciones_fila, names(HD_ACCIONES_FILA))
  if (length(desconocidas))
    stop(sprintf("hd_table: unknown row action(s): %s. Valid: %s.",
                 paste(desconocidas, collapse = ", "), paste(names(HD_ACCIONES_FILA), collapse = ", ")),
         call. = FALSE)
  names(HD_ACCIONES_FILA)[names(HD_ACCIONES_FILA) %in% acciones_fila]   # catalog order
}

hd_acciones_fila_meta <- function(acciones) {
  lapply(acciones, function(a) {
    def <- HD_ACCIONES_FILA[[a]]
    list(accion = a, icono = def$icono, titulo = tr(def$clave), estilo = def$estilo,
         confirmar = isTRUE(def$confirmar), peligro = isTRUE(def$peligro))
  })
}

# ── Bar actions (barra_acc) ─────────────────────────────────────────────────
# acciones_barra = list(list(accion = "mandar_papelera", texto = <key or text>,
#                            confirmar = TRUE, peligro = TRUE, sin_seleccion = FALSE), ...)
# Shown above the table while rows are marked; they send the marked ids in
# visible order. sin_seleccion = TRUE: always shown, sends the visible
# (unlocked) ids -- "Agregar todo".
hd_acciones_barra_validar <- function(acciones_barra) {
  if (is.null(acciones_barra)) return(NULL)
  if (!is.list(acciones_barra) || !length(acciones_barra))
    stop("hd_table: `acciones_barra` must be a list of list(accion, texto, ...).", call. = FALSE)
  campos <- c("accion", "texto", "confirmar", "peligro", "sin_seleccion")
  out <- lapply(seq_along(acciones_barra), function(k) {
    a <- acciones_barra[[k]]
    if (!is.list(a) || !is.character(a$accion) || length(a$accion) != 1L || !nzchar(a$accion))
      stop(sprintf("hd_table: acciones_barra[[%d]] needs `accion` (one name).", k), call. = FALSE)
    if (!is.character(a$texto) || length(a$texto) != 1L || !nzchar(a$texto))
      stop(sprintf("hd_table: acciones_barra '%s' needs `texto` (a dictionary key or text).", a$accion), call. = FALSE)
    otros <- setdiff(names(a), campos)
    if (length(otros))
      stop(sprintf("hd_table: acciones_barra '%s': unknown field(s) %s.", a$accion, paste(otros, collapse = ", ")),
           call. = FALSE)
    list(accion = a$accion, texto = a$texto, confirmar = isTRUE(a$confirmar),
         peligro = isTRUE(a$peligro), sin_seleccion = isTRUE(a$sin_seleccion))
  })
  nombres <- vapply(out, `[[`, character(1), "accion")
  if (anyDuplicated(nombres))
    stop(sprintf("hd_table: acciones_barra repeats '%s'.", nombres[duplicated(nombres)][1]), call. = FALSE)
  if ("detalle" %in% nombres)
    stop("hd_table: 'detalle' is reserved for the row double click.", call. = FALSE)
  out
}

hd_acciones_barra_meta <- function(acciones) {
  lapply(acciones, function(a) utils::modifyList(a, list(texto = .hd_texto(a$texto))))
}

# Confirmation can't be dropped silently: an action that asks for it on a
# table without the `confirmar` piece is a programming error.
.hd_exigir_confirmar <- function(tabla, piezas, quien) {
  if (!"confirmar" %in% piezas)
    stop(sprintf("hd_table '%s': %s asks for confirmation but the table doesn't have the 'confirmar' piece.",
                 tabla, quien), call. = FALSE)
}

# ── Export (excel) ──────────────────────────────────────────────────────────
# Text a spreadsheet would run as a formula gets an apostrophe in front:
# titles and notes are typed by people and must never execute on opening.
hd_neutralizar <- function(x) {
  x <- as.character(x)
  peligro <- !is.na(x) & grepl("^[=+@\t\r-]", x)
  x[peligro] <- paste0("'", x[peligro])
  x
}

# Writes the rows `ids` (visible ones, in visible order) with every column,
# titles as the header, and raw typed values: money as numbers, dates as
# real dates, the rest as (neutralized) text.
hd_excel_escribir <- function(datos, columnas, ids, id_col, archivo, hoja = "datos") {
  datos <- as.data.frame(datos, stringsAsFactors = FALSE)
  filas <- match(as.character(ids), as.character(datos[[id_col]]))
  filas <- filas[!is.na(filas)]
  d <- datos[filas, , drop = FALSE]
  salida <- lapply(columnas, function(col) {
    v <- col$valores(d[[col$campo]])
    switch(col$tipo,
      dinero = v$orden,
      fecha  = as.Date(ifelse(nzchar(v$copiar), v$copiar, NA_character_)),
      hd_neutralizar(v$copiar))
  })
  titulos <- vapply(columnas, function(col) .hd_texto(col$titulo), character(1))
  salida <- as.data.frame(salida, stringsAsFactors = FALSE, optional = TRUE)
  names(salida) <- make.unique(titulos)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, substr(hoja, 1, 31))
  openxlsx::writeData(wb, 1, salida, headerStyle = openxlsx::createStyle(textDecoration = "bold"))
  for (j in seq_along(columnas)) {
    estilo <- switch(columnas[[j]]$tipo,
      dinero = openxlsx::createStyle(numFmt = "#,##0.00"),
      fecha  = openxlsx::createStyle(numFmt = "yyyy-mm-dd"),
      NULL)
    if (!is.null(estilo) && nrow(salida))
      openxlsx::addStyle(wb, 1, estilo, rows = seq_len(nrow(salida)) + 1L, cols = j, stack = TRUE)
  }
  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)
  invisible(archivo)
}

hd_table_server <- function(id, tabla, datos, columnas, piezas = NULL, id_col = "id",
                            fichas = NULL, acciones_fila = NULL, acciones_barra = NULL,
                            filtros = NULL, periodo = NULL, aviso = NULL, encabezado = NULL) {
  piezas_tabla <- hd_piezas_de(tabla, piezas)
  piezas_motor <- hd_piezas_motor(tabla, piezas_tabla)
  # columnas: a list of col_*(), or a reactive returning one -- a table pasted
  # from Excel has other columns with each paste and with the header switch.
  columnas_r   <- if (is.function(columnas)) columnas else local({ cols <- columnas; function() cols })
  cols_fijas   <- if (is.function(columnas)) list() else columnas
  if (!is.function(columnas)) hd_columnas_meta(columnas)   # fails early on a bad spec
  contador     <- hd_contador_clave(tabla)
  fichas       <- .hd_sin_pieza(tabla, "fichas", "fichas", hd_fichas_validar(fichas), piezas_tabla)
  filtros      <- .hd_sin_pieza(tabla, "filtros", "filtros", hd_filtros_validar(filtros), piezas_tabla)
  periodo      <- .hd_sin_pieza(tabla, "periodo", "nav_periodo", hd_periodo_validar(periodo, cols_fijas), piezas_tabla)
  if (!is.null(aviso) && !is.function(aviso))
    stop("hd_table: `aviso` must be a reactive returning a text or NULL.", call. = FALSE)
  if (!is.null(encabezado) && !is.function(encabezado))
    stop("hd_table: `encabezado` must be a reactive returning TRUE or FALSE.", call. = FALSE)
  acc_fila     <- .hd_sin_pieza(tabla, "acciones_fila", "acc_fila",
                                hd_acciones_fila_validar(acciones_fila), piezas_tabla)
  acc_fila_meta <- if (length(acc_fila)) hd_acciones_fila_meta(acc_fila) else NULL
  if ("eliminar" %in% acc_fila) .hd_exigir_confirmar(tabla, piezas_tabla, "row action 'eliminar'")
  acc_barra    <- .hd_sin_pieza(tabla, "acciones_barra", "barra_acc",
                                hd_acciones_barra_validar(acciones_barra), piezas_tabla)
  for (a in acc_barra) if (a$confirmar) .hd_exigir_confirmar(tabla, piezas_tabla, sprintf("bar action '%s'", a$accion))
  acc_barra_meta <- if (length(acc_barra)) hd_acciones_barra_meta(acc_barra) else NULL

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    shiny::observe({
      df <- datos()
      shiny::req(!is.null(df))
      cols <- columnas_r()
      if (is.function(columnas) && !is.null(periodo)) hd_periodo_validar(periodo, cols)
      session$sendCustomMessage("hdt-datos", list(
        id       = ns("hdt"),
        tabla    = tabla,
        filas    = hd_filas(df, cols, id_col),
        columnas = hd_columnas_meta(cols),
        piezas   = I(piezas_motor),
        textos   = list(contador = contador),
        fichas   = hd_fichas_payload(fichas, df, cols),
        filtros  = hd_filtros_payload(filtros, df, cols),
        periodo  = hd_periodo_payload(periodo, df),
        # "La primera fila es encabezado": the switch shows what the server
        # decided; toggling it sends accion "encabezado" with ids "si"/"no".
        encabezado = if (!is.null(encabezado)) isTRUE(encabezado()),
        acciones_fila  = acc_fila_meta,
        acciones_barra = acc_barra_meta
      ))
    })

    # A line above the table in the warning tone ("Se pegaron 340 filas; se
    # guardaron 200"), written by the server with tr(); NULL hides it.
    if (!is.null(aviso)) {
      shiny::observe({
        texto <- aviso()
        texto <- if (is.null(texto) || !length(texto) || is.na(texto[1]) || !nzchar(texto[1])) "" else as.character(texto[1])
        session$sendCustomMessage("hdt-nota", list(id = ns("hdt"), texto = texto))
      })
    }

    # Export: the browser sends the visible ids in visible order with a
    # nonce; once they're stored the server answers with the same nonce and
    # only then does the browser start the download, so the file can never
    # come out with another filter or order than the screen.
    if ("excel" %in% piezas_tabla) {
      vista <- shiny::reactiveVal(NULL)
      shiny::observeEvent(input$vista, {
        v <- input$vista
        vista(as.character(unlist(v$ids)))
        session$sendCustomMessage("hdt-excel", list(id = ns("hdt"), nonce = v$nonce))
      })
      output$excel <- shiny::downloadHandler(
        filename = function() paste0(tabla, "_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"),
        content  = function(file) {
          hd_excel_escribir(shiny::isolate(datos()), shiny::isolate(columnas_r()), shiny::isolate(vista()) %||% character(0),
                            id_col, file, hoja = tabla)
        }
      )
      # The link is hidden (the visible button drives it), and Shiny never
      # gives a hidden download link its URL unless told not to suspend it.
      shiny::outputOptions(output, "excel", suspendWhenHidden = FALSE)
    }

    list(
      seleccion = shiny::reactive(as.character(unlist(input$sel)) %||% character(0)),
      # The month the browser shows; until it reports one, the initial month.
      periodo   = shiny::reactive({
        if (is.null(periodo)) return(NULL)
        m <- input$periodo
        if (is.character(m) && length(m) == 1L && grepl("^[0-9]{4}-[0-9]{2}$", m)) m else periodo$inicial
      }),
      accion    = shiny::reactive({
        a <- input$accion
        if (is.null(a)) return(NULL)
        list(accion = as.character(a$accion), ids = as.character(unlist(a$ids)), nonce = a$nonce)
      })
    )
  })
}
