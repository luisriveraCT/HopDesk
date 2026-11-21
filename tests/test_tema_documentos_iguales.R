# =============================================================================
# tests/test_tema_documentos_iguales.R
# TEMA stage 4b: the Flujo's documents (Word, PDF's HTML, Excel) take their
# colours from the theme registry (.cf_doc_colores()). Chrome never sees them,
# so this is the documents' "Clásico the same": each checkout builds the three
# documents from one fixture in a clean R process, and the colours each one
# writes (Word: w:color / w:fill; HTML: hex and rgb[a](); Excel: styles.xml)
# are counted. The branch writes the same colours as the base, as many times,
# except the tags: A, B and C are etq-a/b/c-texto, the screen's (Luis' rule),
# where the documents had their own dark reds and amber.
#
# Also: under Marca the three documents follow the theme (the Excel through
# the real worker, the theme travelling in its snapshot).
#
#   TEMA_BASE=<checkout before 4b> Rscript --vanilla tests/test_tema_documentos_iguales.R
# Without TEMA_BASE only the branch's own checks run.
# =============================================================================
cat("=== test_tema_documentos_iguales ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
RAMA <- normalizePath(".", winslash = "/")
BASE <- Sys.getenv("TEMA_BASE")
if (nzchar(BASE)) BASE <- normalizePath(BASE, winslash = "/")

# The three documents of one checkout, in a clean process. The fixture is the
# branch's (tests/helpers/flujo_datos_prueba.R) plus one invoice tagged urgent
# AND important (B), so A, B and C all show.
documentos <- function(raiz, tema = NULL) callr::r(function(raiz, ayudante, tema, sal) {
  setwd(raiz)
  suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
  cargar <- function(archivo, nombres) for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
  assign("%||%", function(a, b) if (is.null(a) || length(a) == 0L) b else a, envir = globalenv())
  for (f in c("R/i18n.R", "R/monedas.R", "R/zona.R")) if (file.exists(f)) source(f)
  cargar("R/persistence.R", "tag_label")
  cargar("R/data_pipeline.R", c("etiquetas_por_factura", ".etiquetas_doc_unica", ".politica_doc", ".politica_doc_unica"))
  for (f in c("R/tema_registro.R", "R/fx_banxico.R", "R/fx_tipos.R", "R/hd_registro.R", "R/hd_piezas.R",
              "R/hd_columnas.R", "R/hd_table.R", "R/cashflow_export_module.R"))
    if (file.exists(f)) suppressMessages(source(f))
  source(ayudante)
  lunes <- as.Date("2026-11-02")
  d <- flujo_datos_prueba(lunes)
  d$tags <- dplyr::bind_rows(d$tags, tibble::tibble(ledger = "AP", Empresa = "NETWORKS TRANS", Moneda = "MXN",
                                                    Documento = "AP-PROVEEDOR UNO-0", tag = c("urgent", "important")))
  ed <- build_cashflow_export_data(d$df, d$tags, d$empresas, lunes, lunes + 6L, "auto", "separate", "MXN", c())
  extra <- if (is.null(tema)) list() else list(tema = tema)
  args <- c(list(ed, "NG", "Grupo", "Sin IC", "prueba"), extra)
  # in the caller's folder: this process's tempdir goes when it ends
  docx <- file.path(sal, "doc.docx"); xlsx <- file.path(sal, "libro.xlsx"); html <- file.path(sal, "doc.html")
  print(do.call(generate_cashflow_word_doc, args), target = docx)
  file.copy(do.call(generate_cashflow_html_doc, args), html, overwrite = TRUE)
  openxlsx::saveWorkbook(do.call(generate_cashflow_excel_wb, args), xlsx, overwrite = TRUE)
  list(docx = docx, html = html, xlsx = xlsx)
}, args = list(raiz = raiz, ayudante = file.path(RAMA, "tests/helpers/flujo_datos_prueba.R"), tema = tema,
               sal = { s <- tempfile("docs_"); dir.create(s); s }))

hex6 <- function(x) {
  x <- toupper(sub("^#", "", x))
  ifelse(nchar(x) == 3L, paste0(substr(x, 1, 1), substr(x, 1, 1), substr(x, 2, 2), substr(x, 2, 2),
                                substr(x, 3, 3), substr(x, 3, 3)), x)
}
de_zip <- function(zip, dentro) {
  d <- tempfile(); dir.create(d); utils::unzip(zip, files = dentro, exdir = d)
  paste(readLines(file.path(d, dentro), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
todas <- function(txt, rx) { m <- regmatches(txt, gregexpr(rx, txt, perl = TRUE))[[1]]; sub(rx, "\\1", m, perl = TRUE) }
colores <- function(docs) {
  w <- de_zip(docs$docx, "word/document.xml")
  word <- c(todas(w, '<w:color w:val="([0-9A-Fa-f]{6})"'), todas(w, 'w:fill="([0-9A-Fa-f]{6})"'),
            todas(w, '<w:(?:top|bottom|left|right)\\b[^>]*w:color="([0-9A-Fa-f]{6})"'))
  h <- paste(readLines(docs$html, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  rgb <- regmatches(h, gregexpr("rgba?\\(\\s*\\d+\\s*,\\s*\\d+\\s*,\\s*\\d+\\s*(?:,\\s*[0-9.]+\\s*)?\\)", h, perl = TRUE))[[1]]
  rgb <- vapply(regmatches(rgb, gregexpr("\\d+(?:\\.\\d+)?", rgb)), function(v)
    if (length(v) == 4L && as.numeric(v[4]) == 0) "TRANSPARENTE" else sprintf("%02X%02X%02X", as.integer(v[1]), as.integer(v[2]), as.integer(v[3])), "")
  # a CSS "white" is #FFFFFF (htmltools::save_html(background = "white") before TEMA 4b)
  blancos <- rep("FFFFFF", length(todas(h, "(?i)(?:background(?:-color)?|color)\\s*:\\s*(white)\\b")))
  html <- c(todas(h, "(?<![&\\w])#([0-9A-Fa-f]{6}|[0-9A-Fa-f]{3})(?![0-9A-Za-z_-])"), rgb, blancos)
  x <- de_zip(docs$xlsx, "xl/styles.xml")
  excel <- todas(x, 'rgb="FF([0-9A-Fa-f]{6})"')
  lapply(list(word = word, html = html, excel = excel), function(v) table(hex6(v)))
}
igual <- function(a, b) {
  k <- union(names(a), names(b))
  a <- a[k]; b <- b[k]; a[is.na(a)] <- 0L; b[is.na(b)] <- 0L
  dif <- k[as.integer(a) != as.integer(b)]
  if (length(dif)) message("    distintos: ", paste(sprintf("%s %d->%d", dif, as.integer(a[dif]), as.integer(b[dif])), collapse = ", "))
  !length(dif)
}

reg <- new.env(); local({ `%||%` <- function(a, b) if (is.null(a)) b else a; sys.source("R/tema_registro.R", envir = reg) })
reg$`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
ETQ <- c(A = hex6(reg$tema_color("etq-a-texto", "clasico")), B = hex6(reg$tema_color("etq-b-texto", "clasico")),
         C = hex6(reg$tema_color("etq-c-texto", "clasico")))
VIEJAS <- c(A = "8B1A1A", B = "8B0000", C = "8B6914")   # the documents' own tag colours before TEMA 4b
# TEMA_DOC_ETIQUETAS_VIEJAS=1: a checkout between the two 4b commits (roles,
# tags not yet moved) must equal the base exactly, tags included.
if (identical(Sys.getenv("TEMA_DOC_ETIQUETAS_VIEJAS"), "1")) ETQ <- VIEJAS

rama <- colores(documentos(RAMA))
ok("the branch builds the three documents (Word, HTML, Excel) and each writes colours",
   all(vapply(rama, function(t) sum(t) > 0, TRUE)))
for (d in names(rama)) {
  ok(sprintf("%s: tags A, B and C in the screen's colours (etq-a/b/c-texto: %s)", d, paste(ETQ, collapse = ", ")),
     all(ETQ %in% names(rama[[d]])))
  if (!identical(ETQ, VIEJAS))
    ok(sprintf("%s: none of the documents' old tag colours (%s)", d, paste(VIEJAS, collapse = ", ")),
       !any(VIEJAS %in% names(rama[[d]])))
}

if (nzchar(BASE)) {
  base <- colores(documentos(BASE))
  for (d in names(base)) {
    b <- base[[d]]
    # the base with its tag colours turned into the screen's: what the branch must write
    nm <- names(b); for (k in names(VIEJAS)) nm[nm == VIEJAS[[k]]] <- ETQ[[k]]
    esperado <- tapply(as.integer(b), nm, sum)
    ok(sprintf("%s: under Clásico every colour is the base's, as many times, except the tags (%d colours, %d uses)",
               d, length(esperado), sum(esperado)),
       igual(esperado, rama[[d]]))
  }
} else cat(" (sin TEMA_BASE: no se compara contra la base)\n")

# Under Marca the documents follow the theme: the header colour is Marca's.
enc_marca <- hex6(reg$tema_color("encabezado", "marca")); enc_clasico <- hex6(reg$tema_color("encabezado", "clasico"))
marca <- colores(documentos(RAMA, tema = "marca"))
for (d in names(marca))
  ok(sprintf("%s under Marca: the header is Marca's (%s), not Clásico's (%s)", d, enc_marca, enc_clasico),
     enc_marca %in% names(marca[[d]]) && !(enc_clasico %in% names(marca[[d]])))

# The Excel export runs in a separate process (.cf_excel_worker): the theme
# travels in its snapshot.
worker <- function(tema) {
  snap <- tempfile(fileext = ".rds"); out <- tempfile(fileext = ".xlsx")
  lunes <- as.Date("2026-11-02")
  saveRDS(list(combined_df = tibble::tibble(Tipo = c("AR", "AP"), Parte = c("CLIENTE ALFA", "PROVEEDOR UNO"),
                                            Empresa = "NT", Moneda = "MXN", FechaEff = lunes + c(1, 2),
                                            `Saldo vencido` = c(1500, 400), source = "sap", Documento = c("F1", "F2")),
               tags_df = NULL, emp_sel = "NT", date_from = lunes, date_to = lunes + 6, grouping = "daily",
               currency_mode = "separate", base_cur = "MXN", fx_rates = c(), initials = "NT",
               ic_label = "Sin IC", user_name = "prueba", g_name = "Grupo", g_logo_raw = NULL, g_logo_ext = "png",
               lang_primary = "es", lang_secondary = NULL, tema = tema), snap)
  e <- new.env()
  for (ex in parse(file.path(RAMA, "R/cashflow_export_module.R")))
    if (is.call(ex) && identical(as.character(ex[[2]])[1], ".cf_excel_worker")) eval(ex, envir = e)
  callr::r(e$.cf_excel_worker, args = list(snapshot_path = snap, output_path = out, app_dir = RAMA))
  table(hex6(todas(de_zip(out, "xl/styles.xml"), 'rgb="FF([0-9A-Fa-f]{6})"')))
}
w_marca <- worker("marca"); w_clasico <- worker(NULL)
ok("the Excel worker with a Marca snapshot: Marca's header colour",
   enc_marca %in% names(w_marca) && !(enc_clasico %in% names(w_marca)))
ok("... and without a theme in the snapshot (an older session): Clásico's",
   enc_clasico %in% names(w_clasico))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
