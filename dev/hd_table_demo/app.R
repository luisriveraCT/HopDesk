# hd_table demo: shiny::runApp("dev/hd_table_demo") from the repo root.
# Generated data only: never touches S3 and never loads app.R.
#
# Query string (for tests): ?n=60&seed=1&alto=300px&periodo=0
#   n        rows in the class-A table (default 300; the selector offers 5,000)
#   seed     random seed for the generated data (default 1)
#   alto     height of the class-A table (default "auto")
#   periodo  0: no month navigation, no pinned rows and no empty dates in the
#            class-A table (the T02/T03a tests count every row)
#
# Bootstrap 5 with the app's own theme (flatly), so sizes are the ones
# HopDesk will show.

library(shiny)

# Works both from runApp("dev/hd_table_demo") (wd = this folder) and when
# sourced from the repo root.
.raiz <- normalizePath(if (file.exists("R/hd_table.R")) "." else "../..", winslash = "/")
# The engine's files are loaded into the global environment (as global.R
# does in the app), so %||% has to live there too.
assign("%||%", function(x, y) {
  if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
}, envir = globalenv())
local({
  owd <- setwd(.raiz); on.exit(setwd(owd))
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
    sys.source(f, envir = globalenv())
})
# i18n.R reads www/i18n/*.json relative to the working directory, and
# runApp() runs from this folder.
assign(".i18n_dir", file.path(.raiz, "www", "i18n"), envir = globalenv())

# Class-A switches: copiar_tabla is a class-E piece (see the third table).
PIEZAS_T02 <- setdiff(HD_PIEZAS_MOTOR, "copiar_tabla")

# ── Generated data ──────────────────────────────────────────────────────────
demo_facturas <- function(n, seed = 1, periodo = TRUE) {
  set.seed(seed)
  nombres <- c("Transportes del Bajío", "Aceros Monterrey", "Logística Querétaro",
               '<b>Renta</b> & "Bodega"', "Servicios Integrales de Mantenimiento Industrial del Norte, S.A. de C.V.",
               "Papelería Ñandú", "Árbol Verde Agrícola", "Fletes & Maniobras \"El Güero\"",
               "=HIPERVINCULO(\"x\")")   # copying / exporting it must never run as a formula
  # Runs of rows of the same type, for Shift + click.
  tipos <- character(0)
  while (length(tipos) < n) tipos <- c(tipos, rep(sample(c("cobro", "pago"), 1), sample(3:8, 1)))
  tipos <- tipos[seq_len(n)]
  data.frame(
    id         = sprintf("F%05d", seq_len(n)),
    Empresa    = sample(c("NTS", "NCS", "NTSBB"), n, TRUE),
    Parte      = sample(nombres, n, TRUE),
    Documento  = sprintf("%d", 100000 + seq_len(n)),
    Importe    = round(runif(n, 500, 250000), 2),
    Moneda     = sample(c("MXN", "MXN", "USD"), n, TRUE),
    Vence      = Sys.Date() - sample(-20:90, n, TRUE),
    Estado     = sample(c("pendiente", "programada", "confirmada"), n, TRUE, prob = c(.6, .3, .1)),
    Etiqueta   = sample(c("", "", "", "Urgente", "Importante"), n, TRUE),
    .tipo      = tipos,
    stringsAsFactors = FALSE
  ) |> transform(.bloqueada = Estado == "confirmada") -> df
  # Provision rows; half of them lose the convert button (.acciones), and
  # only pending rows offer "Crear provisión".
  df$.provision <- seq_len(n) %% 9 == 0
  # Month navigation: every 11th row is pinned (always shown, on top); some
  # rows have no date -- the pinned ones among them still show.
  if (periodo) {
    df$.fijar <- seq_len(n) %% 11 == 0
    df$Vence[seq_len(n) %% 22 == 0 | seq_len(n) %% 23 == 0] <- NA
  }
  df$.acciones <- paste0(ifelse(df$Estado == "pendiente", "crear_provision,", ""), "agregar,editar,detalle,eliminar",
                         ifelse(seq_len(n) %% 18 == 0, "", ",provision"))
  df
}

demo_proveedores <- function(seed = 1) {
  set.seed(seed + 100)
  n <- 60
  data.frame(
    id     = sprintf("P%03d", seq_len(n)),
    Nombre = paste(sample(c("Proveedor", "Distribuidora", "Comercial", "Grupo"), n, TRUE),
                   sample(c("Norte", "Sur", "Bajío", "Pacífico", "<i>Centro</i>"), n, TRUE), seq_len(n)),
    RFC    = sprintf("XAXX%06d%03d", sample(1e5:9e5, n), seq_len(n)),
    Banco  = sample(c("BBVA", "Banorte", "Santander", "BanBajío"), n, TRUE),
    Estado = sample(c("completo", "incompleto"), n, TRUE),
    stringsAsFactors = FALSE
  )
}

col_estado <- col_insignia("Estado", "Estado", mapa = list(
  pendiente  = list(texto = "Pendiente",  tono = "aviso"),
  programada = list(texto = "Programada", tono = "info"),
  confirmada = list(texto = "Confirmada", tono = "exito")))
COLS_A <- list(col_empresa("Empresa"), col_texto("Parte", "Proveedor"), col_texto("Documento", "Documento", recorte = FALSE),
               col_dinero("Importe", "Importe"), col_texto("Moneda", "Moneda", recorte = FALSE),
               col_fecha("Vence", "Vence"), col_estado, col_etiqueta("Etiqueta", "Etiqueta"))
COLS_D <- list(col_texto("Nombre", "Nombre"), col_texto("RFC", "RFC", recorte = FALSE),
               col_texto("Banco", "Banco"),
               col_insignia("Estado", "Estado", mapa = list(
                 completo   = list(texto = "Completo",   tono = "exito"),
                 incompleto = list(texto = "Incompleto", tono = "aviso"))))

# The demo's class-A table is a registry entry whose pieces the switches
# rewrite, so the engine resolves them exactly as for a real table.
piezas_demo <- function(encendidas) {
  cl <- hd_clases$A
  list(nombre = "Demo clase A", clase = "A", archivo = "dev/hd_table_demo/app.R",
       agregar = intersect(encendidas, cl$opcional),
       quitar  = setdiff(intersect(cl$siempre, PIEZAS_T02), encendidas),
       notas = character())
}
# Registry entries for the demo: in memory only, in this R process
# (R/hd_registro.R is never written).
registrar <- function(nombre, def) {
  reg <- get("hd_registro", envir = globalenv())
  reg[[nombre]] <- def
  assign("hd_registro", reg, envir = globalenv())
}
registrar("demo_d", list(nombre = "Demo clase D", clase = "D", archivo = "dev/hd_table_demo/app.R",
                         agregar = character(), quitar = character(), notas = character()))
# Like evidencia_excel, plus copiar_tabla (not in the generated registry yet).
registrar("demo_e", list(nombre = "Demo clase E · tabla pegada", clase = "E", archivo = "dev/hd_table_demo/app.R",
                         agregar = c("recorte", "copiar_tabla"), quitar = "insignias", notas = character()))

# A table pasted from Excel: text exactly as it came, first row maybe a header.
PEGADA <- rbind(
  c("Fecha", "Folio", "Concepto", "Importe", "Nota"),
  c("01/02", "007", "Renta de bodega", "15,000.00", "=SUMA(A1)"),
  c("15/02", "0012", "Fletes Querétaro - Monterrey, ida y vuelta con maniobras de carga y descarga", "8,450.50", "+52 55 1234 5678"),
  c("28/02", "00099", "<b>Mantenimiento</b>", "-1,200.00", "@pendiente"),
  c("03/03", "1e5", "Papelería", "320", "-"),
  c("10/03", "0001", "Árbol \"El Güero\"", "1,000", "")
)
# Pasted cells as text columns (c1, c2, ...); titles from the first row or
# "Columna n".
demo_pegada <- function(encabezado) {
  filas <- if (encabezado) PEGADA[-1, , drop = FALSE] else PEGADA
  df <- as.data.frame(filas, stringsAsFactors = FALSE)
  names(df) <- paste0("c", seq_len(ncol(df)))
  df$id <- as.character(seq_len(nrow(df)))
  titulos <- if (encabezado) PEGADA[1, ] else paste("Columna", seq_len(ncol(PEGADA)))
  list(datos = df, columnas = lapply(seq_len(ncol(PEGADA)), function(j) col_texto(paste0("c", j), titulos[j])))
}

# ── UI ──────────────────────────────────────────────────────────────────────
ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tags$head(tags$style(HTML("
    body { padding: 12px 16px; }
    .demo-grid { display: grid; grid-template-columns: 220px minmax(0, 1fr); gap: 16px; }
    .demo-lado { font-size: .82rem; } .demo-lado .checkbox { margin: 2px 0; }
    .demo-panel { font-size: .78rem; max-height: 140px; overflow: auto; }
    @media (max-width: 800px) { .demo-grid { grid-template-columns: 1fr; } }"))),
  h4("hd_table · demo"),
  div(class = "demo-grid",
    div(class = "demo-lado",
      selectInput("n", "Filas (clase A)", c(300, 1000, 5000), width = "100%"),
      actionButton("cambiar", "Cambiar datos", class = "btn-sm btn-primary"),
      actionButton("solo_mxn", "Solo MXN / todas", class = "btn-sm btn-outline-secondary"),
      tags$hr(),
      checkboxGroupInput("piezas", "Piezas (clase A)", choices = PIEZAS_T02,
                         selected = setdiff(PIEZAS_T02, "casillas"))
    ),
    div(
      uiOutput("tabla_a"),
      tags$h6("seleccion() / accion() · clase A", class = "mt-2"),
      verbatimTextOutput("panel_a", placeholder = TRUE),
      tags$h6("Cada accion() que llega · clase A"),
      verbatimTextOutput("log_a", placeholder = TRUE),
      tags$h5("Clase D · catálogo", class = "mt-3"),
      hd_table_ui("prov", alto = "220px"),
      verbatimTextOutput("panel_d", placeholder = TRUE),
      tags$h5("Clase E · tabla pegada de Excel", class = "mt-3"),
      actionButton("aviso_e", "Aviso sí / no", class = "btn-sm btn-outline-secondary mb-2"),
      hd_table_ui("pegada"),
      verbatimTextOutput("panel_e", placeholder = TRUE)
    )
  )
)

# ── Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  q <- shiny::isolate(parseQueryString(session$clientData$url_search))
  seed <- as.integer(q$seed %||% 1)
  alto <- q$alto %||% "auto"
  con_periodo <- !identical(q$periodo, "0")
  if (!is.null(q$n)) updateSelectInput(session, "n", choices = unique(c(as.integer(q$n), 300, 1000, 5000)),
                                       selected = as.integer(q$n))

  cambios <- reactiveVal(0)
  observeEvent(input$cambiar, cambios(cambios() + 1))
  # ?n= wins from the very first paint (the selector's own default of 300
  # would otherwise paint first and be replaced a moment later); after that,
  # the selector.
  n_filas <- reactiveVal(as.integer(q$n %||% 300))
  observeEvent(input$n, ignoreInit = TRUE, n_filas(as.integer(input$n)))

  # "Solo MXN": drops the USD rows (a filter value that disappears); again,
  # brings them back.
  solo_mxn <- reactiveVal(FALSE)
  observeEvent(input$solo_mxn, solo_mxn(!solo_mxn()))

  # "Cambiar datos": same ids, new amounts and states.
  datos_a <- reactive({
    n <- n_filas()
    df <- demo_facturas(n, seed, con_periodo)
    k <- cambios()
    if (k > 0) {
      set.seed(seed + k)
      df$Importe <- round(df$Importe * runif(n, .5, 1.5), 2)
    }
    if (solo_mxn()) df <- df[df$Moneda == "MXN", ]
    df
  })

  version <- reactiveVal(0)
  tabla_a <- reactiveVal(NULL)
  observeEvent(input$piezas, ignoreNULL = FALSE, {
    v <- version() + 1
    version(v)
    nombre <- paste0("demo_a_", v)
    registrar(nombre, piezas_demo(input$piezas %||% character(0)))
    p <- input$piezas %||% character(0)
    # Actions that confirm need the confirmar piece (the engine stops
    # otherwise); with it switched off the demo just leaves them out.
    con_conf <- "confirmar" %in% p
    acc_fila <- c("crear_provision", "agregar", "editar", "provision", "detalle", if (con_conf) "eliminar")
    acc_barra <- c(
      if (con_conf) list(list(accion = "mandar_papelera", texto = "tablas.accion.papelera", confirmar = TRUE, peligro = TRUE)),
      list(list(accion = "agregar_seleccion", texto = "tablas.accion.agregar_seleccion"),
           list(accion = "agregar_todo", texto = "tablas.accion.agregar_todo", sin_seleccion = TRUE)))
    tabla_a(list(id = paste0("a", v),
                 t  = hd_table_server(paste0("a", v), tabla = nombre, datos = datos_a, columnas = COLS_A,
                                      fichas = list(campo = "Estado", orden = c("pendiente", "programada", "confirmada")),
                                      filtros = list(list(campo = "Moneda"), list(campo = "Empresa")),
                                      periodo = if (con_periodo) list(campo = "Vence", inicial = Sys.Date()),
                                      acciones_fila = acc_fila, acciones_barra = acc_barra)))
  })
  # Every accion() that arrives, newest first.
  registro <- reactiveVal(character(0))
  observeEvent(tabla_a()$t$accion(), {
    a <- tabla_a()$t$accion()
    linea <- sprintf("%s  %s  [%d] %s", format(Sys.time(), "%H:%M:%S"), a$accion, length(a$ids),
                     paste(utils::head(a$ids, 8), collapse = " "))
    registro(utils::head(c(linea, registro()), 12))
  })
  output$log_a <- renderPrint(cat(registro(), sep = "\n"))
  output$tabla_a <- renderUI({
    req(tabla_a())
    hd_table_ui(tabla_a()$id, alto = alto)
  })
  # Every id, in order (str() would truncate long selections).
  panel <- function(t) {
    sel <- t$seleccion()
    cat("seleccion (", length(sel), "): ", paste(sel, collapse = " "), "\n", sep = "")
    cat("accion: "); str(t$accion())
    cat("periodo: ", t$periodo() %||% "NULL", "\n", sep = "")
  }
  output$panel_a <- renderPrint({
    req(tabla_a())
    panel(tabla_a()$t)
  })

  t_d <- hd_table_server("prov", tabla = "demo_d", datos = reactive(demo_proveedores(seed)),
                         columnas = COLS_D,
                         piezas = c("densidad", "sel_fila", "sel_rango", "sel_todos", "casillas"),
                         id_col = "id")
  output$panel_d <- renderPrint(panel(t_d))

  # Class E: the server decides the header (the switch only asks), builds
  # the columns from it, and writes the trimming notice.
  encabezado_e <- reactiveVal(TRUE)
  con_aviso <- reactiveVal(TRUE)
  observeEvent(input$aviso_e, con_aviso(!con_aviso()))
  pegada <- reactive(demo_pegada(encabezado_e()))
  t_e <- hd_table_server("pegada", tabla = "demo_e", datos = reactive(pegada()$datos),
                         columnas = reactive(pegada()$columnas),
                         encabezado = encabezado_e,
                         aviso = reactive(if (con_aviso()) "Se pegaron 340 filas; se guardaron 200"))
  observeEvent(t_e$accion(), {
    a <- t_e$accion()
    if (identical(a$accion, "encabezado")) encabezado_e(identical(a$ids, "si"))
  })
  output$panel_e <- renderPrint({
    cat("encabezado: ", encabezado_e(), "\n", sep = "")
    cat("accion: "); str(t_e$accion())
  })
}

app <- shinyApp(ui, server)

# Serve the repo's www/ (hdtable.js, hdtable.css, i18n.js) next to this
# folder's own: runApp() only serves dev/hd_table_demo/www. Dev-only, so the
# internal staticHandler is acceptable here.
local({
  repo_www <- shiny:::staticHandler(file.path(.raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
