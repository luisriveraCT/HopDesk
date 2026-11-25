# =============================================================================
# tests/test_pantallas_registro.R  (T97a)
# The guardian of docs/diseno/pantallas.tsv, the registry of every screen of
# HopDesk (as hd_registro is for the tables). It sweeps the code without the
# app (scripts/pantallas_barrido.R) and compares both ways:
#   1. a screen in the code that the TSV lacks fails, NAMING the row to add;
#   2. a TSV row whose anchor is gone fails (no ghost rows);
#   3. ids unique and well formed, every padre exists, kinds and diseno valid;
#   4. every table id exists in hd_registro, every hd_registro table is on
#      some screen, and it lives in that screen's family of files;
#   5. every tab TIER_REGISTRY names is a nav: of app.R in the TSV;
#   6. every permiso exists in R/permisos.R;
#   7. it is not blind: a planted modal, tab, download, email and page are
#      found, and a planted missing row is named.
# Run from the repo root: Rscript --vanilla tests/test_pantallas_registro.R
# =============================================================================
cat("=== test_pantallas_registro ===\n\n")
`%||%` <- function(a, b) if (is.null(a)) b else a
source("scripts/pantallas_barrido.R")
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/hd_registro.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
  invisible(isTRUE(result))
}

TSV <- "docs/diseno/pantallas.tsv"
d <- pantallas_leer(TSV)
b <- pantallas_barrer(".")
tablas   <- vapply(hd_registro, function(t) t$archivo %||% "", "")
pestanas <- unique(unlist(lapply(TIER_REGISTRY, function(n) n$pestanas)))
problemas <- pantallas_revisar(d, b, ".", tablas = tablas, pestanas = pestanas, permisos = names(PERMISOS))
if (length(problemas)) {
  cat("\nLo que hay que arreglar en", TSV, "(", length(problemas), "):\n")
  cat(paste0("  - ", problemas, collapse = "\n"), "\n\n")
}

# ── 1–6. The registry against today's code ───────────────────────────────────
ok("1-6 the registry and the code agree (every problem is listed above, with the row to add)",
   !length(problemas))
ok("1b the sweep is not empty: tabs, modals, emails, downloads and pages are found",
   all(c("nav", "modal", "correo", "descarga", "pagina") %in% b$clase) && nrow(b) > 100L)
ok("3b the columns are the standard ones, in order", identical(names(d), PANTALLAS_COLUMNAS))
ok("4b all 59+ tables of hd_registro are known to the check", length(tablas) == length(hd_registro) && length(tablas) > 0L)
ok("5b the tier registry names tabs (Compras: PAN and REQ), so check 5 is not vacuous",
   all(c("PAN", "REQ") %in% pestanas))

# ── 7. Not blind: a planted app ──────────────────────────────────────────────
tmp <- file.path(tempdir(), "pantallas_contraprueba"); unlink(tmp, recursive = TRUE)
dir.create(file.path(tmp, "R"), recursive = TRUE)
writeLines(c(
  'ui <- bslib::page_navbar(bslib::nav_panel("Nueva", value = "NUEVA"))',
  'server <- function(input, output, session) {',
  '  query <- parseQueryString(session$clientData$url_search)',
  '  if (nzchar(query[["portal"]] %||% "")) 1',
  '}'), file.path(tmp, "app.R"))
writeLines(c(
  'plantadoServer <- function(id) shiny::moduleServer(id, function(input, output, session) {',
  '  observeEvent(input$abrir, showModal(modalDialog(title = "Plantado")))',
  '  observeEvent(input$abrir, showModal(modalDialog(title = "Otro del mismo clic")))',
  '  output$bajar <- downloadHandler(filename = "x.csv", content = function(f) NULL)',
  '})',
  '.pedir_algo <- function() hd_modal(titulo = tr("algo.titulo"), "x")',
  'email_plantado_html <- function(x) x',
  'r <- function(query) query[["no_es_pagina"]]'), file.path(tmp, "R", "plantado.R"))
bp <- pantallas_barrer(tmp)
esperado <- c("app.R nav:NUEVA", "app.R pagina:portal",
              "R/plantado.R modal:plantadoServer@input$abrir", "R/plantado.R modal:plantadoServer@input$abrir#2",
              "R/plantado.R descarga:bajar", "R/plantado.R modal:.pedir_algo", "R/plantado.R correo:email_plantado_html")
ok("7a the planted tab, page, modals (two on one click: #2), download, hd_modal and email are all found",
   setequal(paste(bp$archivo, bp$ancla), esperado))
ok("7b a query outside app.R is not a page", !any(bp$archivo == "R/plantado.R" & bp$clase == "pagina"))
reg <- pantallas_borrador(bp)
reg$id <- paste0("x", seq_len(nrow(reg))); reg$padre <- ifelse(reg$tipo %in% c("pestana", "correo", "pagina"), "", "x1")
reg$nombre <- "n"
p0 <- pantallas_revisar(reg, bp, tmp)
ok("7c a complete registry of the planted app has no problems", !length(p0))
p1 <- pantallas_revisar(reg[reg$ancla != "modal:.pedir_algo", ], bp, tmp)
ok("7d a missing row is named: file, anchor, and the TSV line to add (with its title guess)",
   length(p1) == 1L && grepl("R/plantado.R  modal:.pedir_algo", p1, fixed = TRUE) &&
     grepl("<id>\talgo.titulo\tmodal\t<padre>\tR/plantado.R\tmodal:.pedir_algo", p1, fixed = TRUE))
fantasma <- rbind(reg, transform(reg[1, ], id = "x_fantasma", ancla = "modal:yaNoExiste"))
ok("7e a ghost row is named", any(grepl("Fila fantasma: 'x_fantasma'", pantallas_revisar(fantasma, bp, tmp), fixed = TRUE)))
manual <- rbind(reg, transform(reg[1, ], id = "x_panel", tipo = "panel", padre = "x1", archivo = "R/plantado.R", ancla = "fn:.no_existe"),
                     transform(reg[1, ], id = "x_panel2", tipo = "panel", padre = "x1", archivo = "R/plantado.R", ancla = "fn:.pedir_algo"))
pm <- pantallas_revisar(manual, bp, tmp)
ok("7f a hand anchor (fn:) must exist in its file: the missing one fails, the real one passes",
   any(grepl("'x_panel' ", pm, fixed = TRUE)) && !any(grepl("'x_panel2'", pm, fixed = TRUE)))
malo <- reg; malo$padre[2] <- "no_hay"; malo$tipo[3] <- "ventana"; malo$id[4] <- malo$id[5]; malo$tablas[5] <- "no_es_tabla"
pmal <- pantallas_revisar(malo, bp, tmp, tablas = c(t1 = "plantado.R"), pestanas = c("NUEVA", "FALTA"), permisos = "can_x")
ok("7g bad padre, kind, repeated id, unknown table, table on no screen and missing tab are each named",
   any(grepl("padre 'no_hay'", pmal)) && any(grepl("tipo 'ventana'", pmal)) && any(grepl("id repetido", pmal)) &&
     any(grepl("'no_es_tabla' no existe", pmal)) && any(grepl("'t1' (plantado.R) de hd_registro", pmal, fixed = TRUE)) &&
     any(grepl("'FALTA'", pmal)) && !any(grepl("'NUEVA'", pmal)))
unlink(tmp, recursive = TRUE)

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
