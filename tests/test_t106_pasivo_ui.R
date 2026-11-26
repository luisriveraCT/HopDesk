# =============================================================================
# tests/test_t106_pasivo_ui.R
# T106 phase 1: the "Documentos" section of a liability's detail (the
# wizard's Revisión step), R/adjuntos_pasivo_ui.R, in shiny::testServer with
# the in-memory S3. Invented data only.
#   1  LAZY (the Diseñador's ask, T108): opening Revisión reads NOTHING; the
#      list is read once the section is expanded; closed again, no read; a
#      new liability (no id) or a tier that may not see Pasivos: no read
#   2  upload: stored, listed; a refused file says why and stores nothing
#   3  a user without can_manage_pasivos cannot add or trash (nothing stored)
#   4  trash from the list; download gives the stored bytes
#   5  the wizard wiring: one line in the Revisión step (after the notice),
#      one in setup_pasivos_wizard(); global.R sources the four files
#
# Run from project root:  Rscript tests/test_t106_pasivo_ui.R
# =============================================================================
cat("=== test_t106_pasivo_ui ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
suppressPackageStartupMessages(library(shiny))
source("tests/helpers/s3_memoria.R")
source("tests/helpers/server_usuarios_archivos.R")
cargar_server_usuarios()
for (f in c("R/adjuntos_s3.R", "R/adjuntos_registro.R", "R/adjuntos_almacen.R", "R/adjuntos_documentos.R",
            "R/adjuntos_pasivo_ui.R")) suppressMessages(sys.source(f, envir = globalenv()))
s3_memoria_reinstalar()

CID <- "demo"; P1 <- "11111111-aaaa-bbbb-cccc-000000000001"
existe_prueba <- function(client_id, id) id %in% P1
ADJ_ENTIDADES$pasivo$existe <- "existe_prueba"
ve_psv  <- function(t) isTRUE(nivel_reactivo_permitido(t, "pasivos_liabilities_db"))
NIVEL   <- Filter(function(t) ve_psv(t) && isTRUE(permisos_resueltos(t, NULL)[["can_manage_pasivos"]]), niveles_cliente())[1]
SIN_PSV <- Filter(function(t) !ve_psv(t), niveles_cliente())[1]
usuario_rv <- reactiveVal(list(tier = NIVEL, permisos = permisos_resueltos(NIVEL, NULL), user = "operador.ficticio"))
shared <- list(usuario = function() usuario_rv(), effective_client_id = function() CID,
               current_user = function() "operador.ficticio", home_client_id = function() CID)
lecturas <- 0L
.documentos_de_real <- documentos_de
documentos_de <- function(...) { lecturas <<- lecturas + 1L; .documentos_de_real(...) }
pdf <- charToRaw("%PDF-1.4\n1 0 obj << >> endobj\n%%EOF\n")
subida <- function(bytes, nombre) { f <- tempfile(fileext = sub(".*([.][a-z]+)$", "\\1", nombre)); writeBin(bytes, f)
  data.frame(name = nombre, size = length(bytes), type = "", datapath = f, stringsAsFactors = FALSE) }
q <- function(expr) invisible(suppressWarnings(suppressMessages(expr)))
salida <- function(x) tryCatch(paste(as.character(x), collapse = ""), error = function(e) "")

id_rv <- reactiveVal(P1)
srv <- function(input, output, session) {
  r <- setup_adjuntos_pasivo(input, output, session, shared, entidad_id = shiny::reactive(id_rv()))
  session$userData$r <- r
}

q(shiny::testServer(srv, {
  cat("== 1. Lectura perezosa ==\n")
  session$flushReact()
  ok("1.1 al abrir Revisión (sección cerrada) no se lee nada", lecturas == 0L && is.null(session$userData$r$docs()))
  session$setInputs(pdoc_abierto = TRUE); session$flushReact()
  invisible(output$pdoc_lista)
  ok("1.2 al expandirla se lee la lista, una vez", lecturas == 1L)
  session$setInputs(pdoc_abierto = FALSE); session$flushReact()
  n0 <- lecturas; invisible(session$userData$r$docs())
  ok("1.3 cerrada otra vez, nada más se lee", lecturas == n0 && is.null(session$userData$r$docs()))
  id_rv(NULL); session$setInputs(pdoc_abierto = TRUE); session$flushReact()
  ok("1.4 un pasivo nuevo (sin id): dice que se guarde primero y no lee",
     lecturas == n0 && grepl(tr("comun.adjuntos.guardar_primero"), salida(output$pdoc_cuerpo), fixed = TRUE))
  id_rv(P1)
  usuario_rv(list(tier = SIN_PSV, permisos = permisos_resueltos(SIN_PSV, NULL))); session$flushReact()
  n1 <- lecturas; invisible(session$userData$r$docs())
  ok("1.5 un nivel que no ve Pasivos: no lee ni muestra", lecturas == n1 && is.null(session$userData$r$docs()))
  usuario_rv(list(tier = NIVEL, permisos = permisos_resueltos(NIVEL, NULL))); session$flushReact()

  cat("== 2. Subir ==\n")
  session$setInputs(pdoc_tipo = "contrato", pdoc_archivo = subida(pdf, "contrato ficticio.pdf")); session$flushReact()
  d <- session$userData$r$docs()
  ok("2.1 el PDF queda guardado y en la lista, con su tipo", is.data.frame(d) && nrow(d) == 1L && d$tipo_doc == "contrato" &&
       grepl("contrato ficticio.pdf", salida(output$pdoc_lista), fixed = TRUE))
  objetos <- length(grep("/adjuntos/", ls(.mem)))
  session$setInputs(pdoc_archivo = subida(c(charToRaw("MZ"), as.raw(rep(0, 40))), "virus.pdf")); session$flushReact()
  ok("2.2 un .exe renombrado no se guarda", length(grep("/adjuntos/", ls(.mem))) == objetos && nrow(session$userData$r$docs()) == 1L)

  cat("== 3. Sin permiso de editar ==\n")
  sin <- permisos_resueltos(NIVEL, NULL); sin["can_manage_pasivos"] <- FALSE
  usuario_rv(list(tier = NIVEL, permisos = sin)); session$flushReact()
  session$setInputs(pdoc_archivo = subida(pdf, "otro.pdf")); session$flushReact()
  ok("3.1 ve la lista pero no puede subir (nada guardado)", length(grep("/adjuntos/", ls(.mem))) == objetos &&
       nrow(session$userData$r$docs()) == 1L)
  session$setInputs(pdoc_quitar = d$id[1]); session$flushReact()
  ok("3.2 ni mandar a la papelera", nrow(session$userData$r$docs()) == 1L)
  ok("3.3 y no ve el botón de quitar", !grepl("pdoc_quitar", salida(output$pdoc_lista), fixed = TRUE))
  usuario_rv(list(tier = NIVEL, permisos = permisos_resueltos(NIVEL, NULL))); session$flushReact()

  cat("== 4. Descargar y quitar ==\n")
  b <- documento_bytes(CID, d$id[1])
  ok("4.1 la descarga da exactamente los bytes guardados", identical(b$bytes, pdf) && identical(b$ext, "pdf"))
  session$setInputs(pdoc_quitar = d$id[1]); session$flushReact()
  ok("4.2 quitar lo manda a la papelera (la lista queda vacía, el objeto sigue)",
     nrow(session$userData$r$docs()) == 0L && exists(paste0(CID, "/adjuntos/pasivo/", P1, "/", d$id[1], ".pdf"), envir = .mem))
}))

cat("== 5. Cableado en el asistente ==\n")
pasos <- paste(readLines("R/pasivos_wizard_steps.R", warn = FALSE, encoding = "UTF-8"), collapse = "\n")
ok("5.1 una línea en Revisión, después del aviso final (no dentro de los fluidRow de la T109)",
   grepl('shiny::uiOutput("pwiz_revision_notice")\n    ),\n    adjuntos_pasivo_ui()', gsub("\r", "", pasos), fixed = TRUE))
modulo <- readLines("R/pasivos_wizard_module.R", warn = FALSE, encoding = "UTF-8")
ok("5.2 una línea en setup_pasivos_wizard(), con el id del pasivo abierto",
   sum(grepl("setup_adjuntos_pasivo(input, output, session, shared, entidad_id = shiny::reactive(wiz$edit_target_id))", modulo, fixed = TRUE)) == 1L)
g <- readLines("R/global.R", warn = FALSE)
pos <- function(f) grep(sprintf('source("%s")', f), g, fixed = TRUE)
ok("5.3 global.R carga las cuatro piezas después de adjuntos_s3.R",
   all(lengths(lapply(c("R/adjuntos_registro.R", "R/adjuntos_almacen.R", "R/adjuntos_documentos.R", "R/adjuntos_pasivo_ui.R"), pos)) == 1L) &&
     pos("R/adjuntos_registro.R") > pos("R/adjuntos_s3.R"))
ui <- salida(adjuntos_pasivo_ui())
ok("5.4 la sección nace cerrada (details sin open) y avisa al servidor que está cerrada",
   grepl("<details", ui, fixed = TRUE) && !grepl("<details[^>]*[[:space:]]open([[:space:]=>]|$)", ui) && grepl("Shiny.setInputValue('pdoc_abierto', false", ui, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
