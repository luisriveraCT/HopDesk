# hd_table demo: shiny::runApp("dev/hd_table_demo") from the repo root.
# Generated data only: never touches S3 and never loads app.R.
#
# Query string (for tests): ?n=60&seed=1&alto=300px
#   n     rows in the class-A table (default 300; the selector offers 5,000)
#   seed  random seed for the generated data (default 1)

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

PIEZAS_T02 <- HD_PIEZAS_MOTOR

# ── Generated data ──────────────────────────────────────────────────────────
demo_facturas <- function(n, seed = 1) {
  set.seed(seed)
  nombres <- c("Transportes del Bajío", "Aceros Monterrey", "Logística Querétaro",
               '<b>Renta</b> & "Bodega"', "Servicios Integrales de Mantenimiento Industrial del Norte, S.A. de C.V.",
               "Papelería Ñandú", "Árbol Verde Agrícola", "Fletes & Maniobras \"El Güero\"")
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
  ) |> transform(.bloqueada = Estado == "confirmada")
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

# ── UI ──────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { padding: 12px 16px; font-family: system-ui, 'Segoe UI', sans-serif; }
    .demo-grid { display: grid; grid-template-columns: 220px minmax(0, 1fr); gap: 16px; }
    .demo-lado { font-size: .82rem; } .demo-lado .checkbox { margin: 2px 0; }
    .demo-panel { font-size: .78rem; max-height: 140px; overflow: auto; }
    @media (max-width: 800px) { .demo-grid { grid-template-columns: 1fr; } }"))),
  h4("hd_table · demo"),
  div(class = "demo-grid",
    div(class = "demo-lado",
      selectInput("n", "Filas (clase A)", c(300, 1000, 5000), width = "100%"),
      actionButton("cambiar", "Cambiar datos", class = "btn-sm btn-primary"),
      tags$hr(),
      checkboxGroupInput("piezas", "Piezas (clase A)", choices = PIEZAS_T02,
                         selected = setdiff(PIEZAS_T02, "casillas"))
    ),
    div(
      uiOutput("tabla_a"),
      tags$h6("seleccion() / accion() · clase A", class = "mt-2"),
      verbatimTextOutput("panel_a", placeholder = TRUE),
      tags$h5("Clase D · catálogo", class = "mt-3"),
      hd_table_ui("prov", alto = "220px"),
      verbatimTextOutput("panel_d", placeholder = TRUE)
    )
  )
)

# ── Server ──────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  q <- shiny::isolate(parseQueryString(session$clientData$url_search))
  seed <- as.integer(q$seed %||% 1)
  alto <- q$alto %||% "420px"
  if (!is.null(q$n)) updateSelectInput(session, "n", choices = unique(c(as.integer(q$n), 300, 1000, 5000)),
                                       selected = as.integer(q$n))

  cambios <- reactiveVal(0)
  observeEvent(input$cambiar, cambios(cambios() + 1))

  # "Cambiar datos": same ids, new amounts and states.
  datos_a <- reactive({
    n <- as.integer(input$n %||% q$n %||% 300)
    df <- demo_facturas(n, seed)
    k <- cambios()
    if (k > 0) {
      set.seed(seed + k)
      df$Importe <- round(df$Importe * runif(n, .5, 1.5), 2)
    }
    df
  })

  version <- reactiveVal(0)
  tabla_a <- reactiveVal(NULL)
  observeEvent(input$piezas, ignoreNULL = FALSE, {
    v <- version() + 1
    version(v)
    nombre <- paste0("demo_a_", v)
    registrar(nombre, piezas_demo(input$piezas %||% character(0)))
    tabla_a(list(id = paste0("a", v),
                 t  = hd_table_server(paste0("a", v), tabla = nombre, datos = datos_a, columnas = COLS_A)))
  })
  output$tabla_a <- renderUI({
    req(tabla_a())
    hd_table_ui(tabla_a()$id, alto = alto)
  })
  # Every id, in order (str() would truncate long selections).
  panel <- function(t) {
    sel <- t$seleccion()
    cat("seleccion (", length(sel), "): ", paste(sel, collapse = " "), "\n", sep = "")
    cat("accion: "); str(t$accion())
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
