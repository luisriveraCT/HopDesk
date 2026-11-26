# =============================================================================
# tests/test_t112_prov_tabs.R
# T112: Proveedores' "Importar Excel" tab is shown/hidden by permission only
# once the module's UI is in the page (input$ui_pintada, sent by
# proveedoresUI() every time it is drawn), and again on every redraw.
# Before: proveedoresServer runs with Ajustes whether or not the section is
# open, so show/hideTab went to a tabset not yet there -- the console error
# "There is no tabsetPanel ... 'prov_mod-prov_tabs'" (staff at home, seen in
# Chrome in T106) -- and, drawn later, the tab stayed visible to whoever may
# not write the catalog.
#   1  before the UI is drawn: no show/hideTab at all
#   2  drawn, without can_manage_proveedores: hideTab("importar")
#   3  drawn again: hidden again (the new DOM gets it too)
#   4  the permission arrives (ver como, a jump): showTab("importar")
#
# Run from project root:  Rscript tests/test_t112_prov_tabs.R
# =============================================================================
cat("=== test_t112_prov_tabs ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") } else { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n") }
}
source("tests/helpers/proveedores_testserver.R")
# The module runs in globalenv: these are found before shiny's.
llamadas <- character()
assign("showTab", function(inputId, target, ...) { llamadas <<- c(llamadas, paste("show", inputId, target)); invisible() }, envir = globalenv())
assign("hideTab", function(inputId, target, ...) { llamadas <<- c(llamadas, paste("hide", inputId, target)); invisible() }, envir = globalenv())
assign("puede", function(usuario, permiso, ...) isTRUE(usuario$escribe), envir = globalenv())

usuario <- reactiveVal(list(escribe = FALSE))
shared <- list(proveedores_db = reactiveVal(prov_fila("p-1", "PROVEEDOR FICTICIO", "012180001234567891")),
               proveedores_inactivos_db = reactiveVal(prov_fila("p-0", "X", "0")[0, ]),
               company_map = reactive(c(NTS = "Ficticia")), effective_client_id = function() "prueba",
               home_client_id = function() "prueba", current_user = function() "prueba",
               usuario = function() usuario(), parte_alias_map_db = reactiveVal(NULL))

suppressWarnings(suppressMessages(shiny::testServer(proveedoresServer, args = list(shared = shared), {
  session$flushReact()
  ok("1 antes de que la UI esté en la página: ni showTab ni hideTab", !length(llamadas))
  session$setInputs(ui_pintada = 0.1)
  ok("2 dibujada, sin can_manage_proveedores: se oculta Importar Excel", identical(llamadas, "hide prov_tabs importar"))
  session$setInputs(ui_pintada = 0.2)
  ok("3 dibujada de nuevo: se vuelve a ocultar (el DOM nuevo también)",
     identical(llamadas, c("hide prov_tabs importar", "hide prov_tabs importar")))
  usuario(list(escribe = TRUE)); session$flushReact()
  ok("4 llega el permiso (ver como, un salto): se muestra", identical(utils::tail(llamadas, 1), "show prov_tabs importar"))
})))

src <- readLines("R/proveedores_module.R", warn = FALSE, encoding = "UTF-8")
i <- grep('if (ok) showTab("prov_tabs", "importar", session = session)', src, fixed = TRUE)
ini <- max(grep("^    observe\\(\\{", src[seq_len(i)]))
ok("5 el observer que muestra/oculta la pestaña empieza esperando ui_pintada",
   length(i) == 1L && any(grepl("req(input$ui_pintada)", src[ini:i], fixed = TRUE)))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
