# =============================================================================
# tests/helpers/matriz_fixture_app.R
# The matrix base (T17a: columna_fija, tono_celda, dinero_compacto) on a
# table shaped like the Flujo de caja / Pasivos: one row per counterparty,
# one money column per period, the name and the company fixed at the left,
# sections by currency. Never touches S3.
#   #m      the matrix: MZ_N rows (default 300) x MZ_C periods (default 180),
#           compact amounts with empty zeros and the sign's tone (the bubble,
#           copiar_celda, shows the full one); "hoy" is
#           shaded and "Estado" tints by a field
#   #menos  the columns are reactive: 90 periods instead of MZ_C
#   #c      3 rows, one compact column, no copiar_celda: the full amount is
#           the cell's title (there is no bubble)
#   #p      the same data with plain columns, none of the new options (it
#           must look like any table did before T17a)
# Amounts are fixed by position: row 1 is 4,495,962 in d001, every 7th
# cell is 0 and every 5th is negative.
# =============================================================================
library(shiny)
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R"))
  sys.source(f, envir = globalenv())
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

N <- as.integer(Sys.getenv("MZ_N", "300"))
C <- as.integer(Sys.getenv("MZ_C", "180"))
periodos <- sprintf("d%03d", seq_len(C))
datos0 <- local({
  d <- data.frame(id = sprintf("r%04d", seq_len(N)),
                  Nombre = sprintf("Contraparte %04d con un nombre largo", seq_len(N)),
                  Empresa = rep(c("NTS", "BOD", "NRS"), length.out = N),
                  Moneda = ifelse(seq_len(N) %% 4 == 0, "USD", "MXN"),
                  Estado = rep(c("aviso", "peligro", "exito", "", "rojo"), length.out = N),
                  stringsAsFactors = FALSE)
  for (k in seq_len(C)) {
    v <- (seq_len(N) * 7919 + k * 104729) %% 2500000 + 0.5
    v[(seq_len(N) + k) %% 7 == 0] <- 0
    neg <- (seq_len(N) + k) %% 5 == 0
    v[neg] <- -v[neg]
    d[[periodos[k]]] <- v
  }
  d$d001[1] <- 4495962
  d$hoy <- d[[periodos[min(3L, C)]]]
  d$est <- d$d002
  d
})

cols_matriz <- function(n) c(
  list(col_texto("Nombre", "Contraparte", fija = TRUE),
       col_empresa("Empresa", fija = TRUE),
       col_dinero("hoy", "Hoy", sumar = FALSE, formato = "compacto", cero = "vacio", tono = "signo", destacar = TRUE),
       col_dinero("est", "Estado", sumar = FALSE, tono = "Estado")),
  lapply(periodos[seq_len(n)], function(p)
    col_dinero(p, p, sumar = FALSE, formato = "compacto", cero = "vacio", tono = "signo")))

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  actionButton("menos", "menos columnas"),
  hd_table_ui("m"),
  hd_table_ui("p"),
  hd_table_ui("c")
)
server <- function(input, output, session) {
  datos <- reactiveVal(datos0)
  n_cols <- reactiveVal(C)
  observeEvent(input$menos, n_cols(min(90L, C)))
  hd_table_server("m", tabla = "flujo_caja", datos = datos, columnas = reactive(cols_matriz(n_cols())),
                  piezas = c("copiar_celda", "tono_celda", "dinero_compacto"), secciones = "Moneda")
  hd_table_server("p", tabla = "flujo_caja", datos = datos,
                  columnas = c(list(col_texto("Nombre", "Contraparte"), col_empresa("Empresa")),
                               lapply(periodos[seq_len(min(10L, C))], function(p) col_dinero(p, p, sumar = FALSE))))
  # Compact without the bubble (no copiar_celda): the full amount on hover.
  hd_table_server("c", tabla = "flujo_caja", datos = reactive(datos0[1:3, ]), piezas = "dinero_compacto",
                  columnas = list(col_texto("Nombre", "Contraparte"), col_dinero("d001", "d001", sumar = FALSE, formato = "compacto")))
}
app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(getwd(), "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
