# =============================================================================
# tests/test_motor_ocultas_con_datos.R
# T38: a table's acciones_ocultas travel inside every hdt-datos message, not
# only in their own hdt-ocultas message. That one goes once, at the session's
# start; an engine that arrives later (the day's modal) lost it while its
# data came again, and a user without can_register_payments saw ＋ and
# "Agregar todo" (reportes/T38_bug_permisos.md). A table without
# acciones_ocultas sends exactly what it sent before.
# The browser side (hdtable.js reads msg.ocultas): test_agenda_sin_permiso_chromote.
# Run from project root:  Rscript tests/test_motor_ocultas_con_datos.R
# HD_TABLE_R = another copy of R/hd_table.R (contraprueba).
# =============================================================================
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
source("R/i18n.R"); source("R/hd_registro.R"); source("R/hd_piezas.R"); source("R/hd_columnas.R")
source(Sys.getenv("HD_TABLE_R", "R/hd_table.R"))
.i18n_dir <- file.path(getwd(), "www", "i18n")

.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}

d <- data.frame(id = c("a", "b"), Parte = c("UNO", "DOS"), Importe = c(10, 20), stringsAsFactors = FALSE)
cols <- list(col_texto("Parte", "tablas.facturas.parte"), col_dinero("Importe", "tablas.facturas.importe"))
permiso <- reactiveVal(FALSE)
mensajes <- list()

srv <- function(id) moduleServer(id, function(input, output, session) {
  rs <- session$rootScope()
  unlockBinding("sendCustomMessage", rs)
  rs$sendCustomMessage <- function(type, message) mensajes[[length(mensajes) + 1L]] <<- list(type = type, msg = message)
  datos <- reactive(d)
  hd_table_server("con", tabla = "detalle_dia_resumen", datos = datos, columnas = cols,
                  acciones_fila = c("agregar", "editar"),
                  acciones_ocultas = reactive(if (permiso()) character(0) else c("agregar", "agregar_todo")))
  hd_table_server("sin", tabla = "detalle_dia_resumen", datos = datos, columnas = cols,
                  acciones_fila = c("agregar", "editar"))
})
datos_de <- function(tabla) Filter(function(m) identical(m$type, "hdt-datos") && grepl(paste0(tabla, "-hdt$"), m$msg$id), mensajes)

testServer(srv, {
  session$flushReact()
  con <- datos_de("con"); sin <- datos_de("sin")
  ok("both tables sent their data", length(con) >= 1L && length(sin) >= 1L)
  ultimo <- con[[length(con)]]$msg
  ok("without the permission, the data message carries the hidden actions (agregar, agregar_todo)",
     identical(as.character(ultimo$ocultas), c("agregar", "agregar_todo")))
  ok("a table without acciones_ocultas: its message has no 'ocultas' (nothing else changes)",
     !"ocultas" %in% names(sin[[length(sin)]]$msg))
  ok("the hdt-ocultas message still goes too",
     any(vapply(mensajes, function(m) identical(m$type, "hdt-ocultas"), logical(1))))

  # The permission arrives: the next data message hides nothing.
  n <- length(con); permiso(TRUE); session$flushReact()
  con <- datos_de("con")
  ok("with the permission, a new data message carries an empty list (shows them again)",
     length(con) > n && identical(length(con[[length(con)]]$msg$ocultas), 0L) &&
       "ocultas" %in% names(con[[length(con)]]$msg))
})

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
