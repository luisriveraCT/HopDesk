# =============================================================================
# scripts/pantallas_barrido.R  (T97a)
# The screen sweep behind docs/diseno/pantallas.tsv: it reads the code (never
# runs the app) and lists every screen it can find by its shape, with a STABLE
# anchor (never a line number). Two users:
#   - tests/test_pantallas_registro.R (the guardian) compares it against the
#     TSV both ways;
#   - run as a script, it writes a draft of the rows the TSV is missing:
#       Rscript --vanilla scripts/pantallas_barrido.R            (prints them)
#       Rscript --vanilla scripts/pantallas_barrido.R --escribir (appends them)
#     A drafted row has id/tipo/archivo/ancla/padre filled; the nombre is a
#     guess from the title and must be read by a person.
# Not under R/: the app never loads it. Base R + jsonlite only.
#
# Anchors the sweep finds (each one lives in the row's `archivo`):
#   nav:<value>               a nav_panel / nav_menu / tabPanel; without a
#                             literal value, its title (text or tr() key)
#   modal:<function>[@<event>] a modalDialog / hd_modal, by the named function
#                             that holds it and, if inside an observeEvent,
#                             that event; a repeat of the same anchor in the
#                             same file gets #2, #3 ... in source order
#   correo:<function>         a function named email_*_html / correo_*_html
#   descarga:<output id>      output$<id> <- downloadHandler(...)
#   pagina:<parameter>        query[["x"]] / query$x: a page outside the app
# Anchors a person writes and the guardian only checks exist (one way):
#   fn:<function>             a function defined in that file (panels)
#   texto:<text>              literal text found in that file (www/ pages)
# =============================================================================

PANTALLAS_COLUMNAS <- c("id", "nombre", "tipo", "padre", "archivo", "ancla",
                        "niveles", "permiso", "tablas", "diseno", "notas")
PANTALLAS_TIPOS  <- c("pestana", "menu", "boton", "pieza", "subpestana", "seccion", "modal", "panel",
                      "correo", "pdf", "excel", "descarga", "pagina")
PANTALLAS_DISENO <- c("pendiente", "en_diseno", "listo")
PANTALLAS_ANCLAS_BARRIDAS <- c("nav", "modal", "correo", "descarga", "pagina")
PANTALLAS_ANCLAS_MANUALES <- c("fn", "texto")

.PB_NAV    <- c("nav_panel", "nav_menu", "tabPanel")
.PB_MODAL  <- c("modalDialog", "hd_modal")
.PB_CORREO <- "^(email|correo)_[A-Za-z0-9_]*_html$"
.PB_EVENTO <- c("observeEvent", "eventReactive")

# The files the sweep reads, relative to `raiz`.
pantallas_archivos <- function(raiz = ".") {
  r <- list.files(file.path(raiz, "R"), pattern = "\\.R$", recursive = TRUE)
  c(if (file.exists(file.path(raiz, "app.R"))) "app.R", file.path("R", sort(r)))
}

.pb_fun <- function(e) {
  f <- e[[1]]
  if (is.symbol(f)) return(as.character(f))
  if (is.call(f) && is.symbol(f[[1]]) && as.character(f[[1]]) %in% c("::", ":::"))
    return(as.character(f[[3]]))
  ""
}
.pb_vacio <- function(a) identical(a, quote(expr = ))
.pb_txt   <- function(a) gsub("\\s+", "", paste(deparse(a, width.cutoff = 500L), collapse = ""))

# A label for a title-ish argument: a literal string, a tr("key") key, or the
# first literal inside tagList()/paste()/HTML() (icon() skipped). NA if none.
.pb_etiqueta <- function(a, clave = FALSE) {
  if (is.null(a) || .pb_vacio(a)) return(NA_character_)
  if (is.character(a) && length(a) == 1L) return(trimws(a))
  if (!is.call(a)) return(NA_character_)
  f <- .pb_fun(a)
  if (f %in% c("tr", "hdT") && length(a) >= 2L && is.character(a[[2]]))
    return(if (clave) a[[2]] else paste0("tr:", a[[2]]))
  if (f %in% c("icon", "bs_icon")) return(NA_character_)
  for (i in seq_along(a)[-1]) {
    if (.pb_vacio(a[[i]])) next
    x <- .pb_etiqueta(a[[i]], clave)
    if (!is.na(x) && nzchar(gsub("[^[:alnum:]]", "", x))) return(x)
  }
  NA_character_
}

.pb_arg <- function(e, nombre, pos = NULL) {
  a <- as.list(e)[-1]
  n <- names(a) %||% rep("", length(a))
  if (nombre %in% n) return(a[[match(nombre, n)]])
  if (!is.null(pos)) { sin <- which(!nzchar(n)); if (length(sin) >= pos) return(a[[sin[pos]]]) }
  NULL
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# Walks one parsed expression; ctx = list(fun, evento). Emits records.
.pb_recorrer <- function(e, ctx, emitir) {
  if (!is.call(e)) return(invisible())
  fn <- .pb_fun(e)
  a  <- as.list(e)[-1]
  if (fn %in% c("<-", "<<-", "=") && length(a) == 2L) {
    lhs <- a[[1]]; rhs <- a[[2]]
    if (is.call(rhs) && identical(rhs[[1]], as.name("function")) && is.symbol(lhs)) {
      ctx$fun <- as.character(lhs); ctx$evento <- NULL
      if (grepl(.PB_CORREO, ctx$fun)) emitir("correo", ctx$fun, NA_character_, ctx)
    }
    if (is.call(rhs) && .pb_fun(rhs) == "downloadHandler" && is.call(lhs) &&
        .pb_fun(lhs) %in% c("$", "[[") && identical(lhs[[2]], as.name("output")))
      emitir("descarga", if (is.symbol(lhs[[3]]) || is.character(lhs[[3]])) as.character(lhs[[3]]) else .pb_txt(lhs[[3]]),
             NA_character_, ctx)
  }
  if (fn %in% .PB_EVENTO) {
    ev <- .pb_arg(e, "eventExpr", 1L)
    if (!is.null(ev)) ctx$evento <- .pb_txt(ev)
  }
  if (fn %in% c("[[", "$") && length(a) == 2L && identical(a[[1]], as.name("query")) &&
      (is.character(a[[2]]) || (fn == "$" && is.symbol(a[[2]]))))
    emitir("pagina", as.character(a[[2]]), NA_character_, ctx)
  if (fn %in% .PB_NAV) {
    v <- .pb_arg(e, "value")
    t <- .pb_arg(e, "title", 1L)
    if (is.call(t) && .pb_fun(t) == "HTML") t <- NULL   # an HTML title (built per company): anchor by function
    clave <- if (is.character(v) && length(v) == 1L) v else .pb_etiqueta(t, clave = TRUE)
    emitir("nav", clave, .pb_etiqueta(t), ctx)
  }
  if (fn %in% .PB_MODAL && !(fn == "hd_modal" && identical(ctx$fun, "hd_modal"))) {
    base <- paste0(ctx$fun %||% "", if (!is.null(ctx$evento)) paste0("@", ctx$evento))
    emitir("modal", base, .pb_etiqueta(.pb_arg(e, if (fn == "hd_modal") "titulo" else "title")), ctx)
  }
  for (i in seq_along(a)) {
    if (.pb_vacio(a[[i]])) next
    if (is.call(a[[i]])) .pb_recorrer(a[[i]], ctx, emitir)
  }
  invisible()
}

# The sweep: one row per screen found, with its anchor.
# Columns: archivo, clase (nav/modal/...), ancla, etiqueta (title guess), funcion.
pantallas_barrer <- function(raiz = ".", archivos = pantallas_archivos(raiz)) {
  filas <- list()
  for (f in archivos) {
    exprs <- parse(file.path(raiz, f), keep.source = FALSE, encoding = "UTF-8")
    emitir <- function(clase, valor, etiqueta, ctx) {
      if (is.na(valor) || !nzchar(valor)) valor <- paste0(ctx$fun %||% "", "#")
      filas[[length(filas) + 1L]] <<- data.frame(
        archivo = f, clase = clase, base = paste0(clase, ":", valor),
        etiqueta = etiqueta %||% NA_character_, funcion = ctx$fun %||% NA_character_,
        stringsAsFactors = FALSE)
    }
    for (e in exprs) .pb_recorrer(e, list(fun = NULL, evento = NULL), emitir)
  }
  if (!length(filas))
    return(data.frame(archivo = character(), clase = character(), ancla = character(),
                      etiqueta = character(), funcion = character(), stringsAsFactors = FALSE))
  d <- do.call(rbind, filas)
  # pagina: only app.R's query (the branches above secure_app; other files
  # have their own `query`), one row per parameter however often it is read
  d <- d[!(d$clase == "pagina" & (d$archivo != "app.R" | duplicated(d[c("archivo", "base")]))), , drop = FALSE]
  # repeats of an anchor in one file: #2, #3 ... in source order
  n <- stats::ave(seq_len(nrow(d)), d$archivo, d$base, FUN = seq_along)
  d$ancla <- ifelse(n == 1L, d$base, paste0(d$base, "#", n))
  d$ancla <- sub("#(#[0-9]+)$", "\\1", sub("#$", "#1", d$ancla))
  rownames(d) <- NULL
  d[c("archivo", "clase", "ancla", "etiqueta", "funcion")]
}

# ── The registry ─────────────────────────────────────────────────────────────
pantallas_leer <- function(ruta = "docs/diseno/pantallas.tsv") {
  d <- utils::read.delim(ruta, colClasses = "character", na.strings = character(),
                         quote = "", comment.char = "", encoding = "UTF-8", check.names = FALSE)
  d[] <- lapply(d, function(x) { x[is.na(x)] <- ""; trimws(x) })
  d
}
.pb_lista <- function(x) { x <- trimws(unlist(strsplit(x %||% "", ";", fixed = TRUE))); x[nzchar(x)] }

# The suggested TSV line for an anchor the registry is missing: what the
# guardian prints, so whoever added the screen knows what to write.
pantallas_fila_sugerida <- function(archivo, ancla, etiqueta = NA_character_) {
  tipo <- switch(sub(":.*", "", ancla), nav = "subpestana", modal = "modal",
                 correo = "correo", descarga = "excel", pagina = "pagina", "")
  nombre <- if (is.na(etiqueta)) "<nombre>" else sub("^tr:", "", etiqueta)
  paste(c("<id>", nombre, tipo, "<padre>", archivo, ancla, "", "", "", "pendiente", ""), collapse = "\t")
}

# Every problem of the registry `d` against the sweep `b`, as sentences.
# `tablas` = ids of hd_registro (with their archivo), `pestanas` = tab ids the
# tier registry names, `permisos` = PERMISOS keys. NULL skips that check.
pantallas_revisar <- function(d, b, raiz = ".", tablas = NULL, pestanas = NULL, permisos = NULL) {
  p <- character()
  falta_col <- setdiff(PANTALLAS_COLUMNAS, names(d))
  if (length(falta_col)) return(paste("Al TSV le faltan columnas:", paste(falta_col, collapse = ", ")))
  if (!identical(names(d), PANTALLAS_COLUMNAS))
    p <- c(p, paste("Las columnas deben ir en este orden:", paste(PANTALLAS_COLUMNAS, collapse = " · ")))
  # 1. new and unregistered
  clave_b <- paste(b$archivo, b$ancla); clave_d <- paste(d$archivo, d$ancla)
  for (i in which(!clave_b %in% clave_d))
    p <- c(p, sprintf("Pantalla sin registrar: %s  %s\n    agrega a docs/diseno/pantallas.tsv la fila:\n    %s",
                      b$archivo[i], b$ancla[i], pantallas_fila_sugerida(b$archivo[i], b$ancla[i], b$etiqueta[i])))
  # 2. registered and gone (only swept kinds; manual kinds must exist)
  kind <- sub(":.*", "", d$ancla)
  for (i in which(kind %in% PANTALLAS_ANCLAS_BARRIDAS & !clave_d %in% clave_b))
    p <- c(p, sprintf("Fila fantasma: '%s' (%s  %s) ya no está en el código; bórrala o corrige su ancla",
                      d$id[i], d$archivo[i], d$ancla[i]))
  for (i in which(kind %in% PANTALLAS_ANCLAS_MANUALES)) {
    ruta <- file.path(raiz, d$archivo[i])
    txt <- if (file.exists(ruta)) paste(readLines(ruta, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
    v <- sub("^[a-z]+:", "", d$ancla[i])
    hay <- if (kind[i] == "fn") grepl(sprintf("(^|\n)\\s*%s\\s*(<-|=)\\s*function", gsub("([.])", "\\\\\\1", v)), txt)
           else grepl(v, txt, fixed = TRUE)
    if (!hay) p <- c(p, sprintf("Fila fantasma: '%s' (%s  %s) no se encuentra en su archivo", d$id[i], d$archivo[i], d$ancla[i]))
  }
  for (i in which(!kind %in% c(PANTALLAS_ANCLAS_BARRIDAS, PANTALLAS_ANCLAS_MANUALES)))
    p <- c(p, sprintf("Ancla de clase desconocida en '%s': %s", d$id[i], d$ancla[i]))
  # 3. ids, parents, kinds
  vacios <- which(!nzchar(d$id))
  if (length(vacios)) p <- c(p, sprintf("Fila sin id (ancla %s)", d$ancla[vacios]))
  dup <- unique(d$id[duplicated(d$id) & nzchar(d$id)])
  if (length(dup)) p <- c(p, paste("id repetido:", dup))
  dupa <- unique(clave_d[duplicated(clave_d)])
  if (length(dupa)) p <- c(p, paste("Ancla registrada dos veces:", dupa))
  malo_id <- d$id[nzchar(d$id) & !grepl("^[a-z0-9_]+(\\.[a-z0-9_]+)*$", d$id)]
  if (length(malo_id)) p <- c(p, paste("id con caracteres no permitidos (minúsculas, números, _ y .):", malo_id))
  sin_padre <- which(nzchar(d$padre) & !d$padre %in% d$id)
  if (length(sin_padre)) p <- c(p, sprintf("'%s' tiene padre '%s', que no existe", d$id[sin_padre], d$padre[sin_padre]))
  raiz_sin <- which(!nzchar(d$padre) & !d$tipo %in% c("pestana", "menu", "boton", "pieza", "pagina", "correo"))
  if (length(raiz_sin)) p <- c(p, sprintf("'%s' (%s) necesita padre", d$id[raiz_sin], d$tipo[raiz_sin]))
  malo_t <- which(!d$tipo %in% PANTALLAS_TIPOS)
  if (length(malo_t)) p <- c(p, sprintf("'%s' tiene tipo '%s'; los tipos son: %s", d$id[malo_t], d$tipo[malo_t], paste(PANTALLAS_TIPOS, collapse = ", ")))
  malo_d <- which(!d$diseno %in% PANTALLAS_DISENO)
  if (length(malo_d)) p <- c(p, sprintf("'%s' tiene diseno '%s'; van: %s", d$id[malo_d], d$diseno[malo_d], paste(PANTALLAS_DISENO, collapse = ", ")))
  sin_nombre <- which(!nzchar(d$nombre) | grepl("^<", d$nombre))
  if (length(sin_nombre)) p <- c(p, sprintf("'%s' no tiene nombre", d$id[sin_nombre]))
  # 4. tables
  if (!is.null(tablas)) {
    usadas <- unlist(lapply(d$tablas, .pb_lista))
    for (t in setdiff(usadas, names(tablas))) p <- c(p, sprintf("La tabla '%s' no existe en hd_registro", t))
    for (t in setdiff(names(tablas), usadas))
      p <- c(p, sprintf("La tabla '%s' (%s) de hd_registro no está en ninguna pantalla: ponla en la columna tablas de la pantalla que la muestra", t, tablas[[t]]))
    familia <- function(i) {
      ids <- d$id[i]; repeat { n <- union(ids, d$id[d$padre %in% ids]); if (length(n) == length(ids)) break; ids <- n }
      a <- d$padre[i]; while (nzchar(a) && a %in% d$id && !a %in% ids) { ids <- c(ids, a); a <- d$padre[match(a, d$id)] }
      basename(d$archivo[d$id %in% ids])
    }
    for (i in which(nzchar(d$tablas) & d$archivo != "app.R")) {
      fam <- familia(i)
      for (t in intersect(.pb_lista(d$tablas[i]), names(tablas))) {
        arch <- trimws(strsplit(tablas[[t]], "·", fixed = TRUE)[[1]])
        if (!length(intersect(arch, fam)))
          p <- c(p, sprintf("La tabla '%s' vive en %s, pero '%s' y su familia están en %s", t, tablas[[t]], d$id[i], paste(unique(fam), collapse = ", ")))
      }
    }
  }
  # 5. tabs the tier registry names
  if (!is.null(pestanas)) {
    navs <- sub("^nav:", "", d$ancla[d$archivo == "app.R" & grepl("^nav:", d$ancla)])
    for (t in setdiff(pestanas, navs)) p <- c(p, sprintf("TIER_REGISTRY nombra la pestaña '%s', que no está como nav:%s de app.R en el TSV", t, t))
  }
  # 6. permissions
  if (!is.null(permisos)) {
    for (k in setdiff(unlist(lapply(d$permiso, .pb_lista)), permisos))
      p <- c(p, sprintf("El permiso '%s' no existe en R/permisos.R", k))
  }
  p
}

# ── Draft rows (first load and the --escribir run) ───────────────────────────
# Which tab a module file belongs to, for the draft's `padre` only. A person
# corrects the draft; the guardian never uses this map.
.PB_RAIZ <- c(
  "R/panorama_module.R" = "pan", "R/panorama_publicar_module.R" = "pan", "R/avisos_alertas_module.R" = "pan",
  "R/ledger_module.R" = "cal", "R/manual_entry_handlers.R" = "cal", "R/notes_handlers.R" = "cal",
  "R/staging_browse_module.R" = "cal", "R/cashflow_preview_module.R" = "cal",
  "R/vencidos_module.R" = "ven", "R/pagar_hoy_module.R" = "ph", "R/bancos_module.R" = "bnc",
  "R/interco_module.R" = "ic", "R/treasury_map_module.R" = "ic",
  "R/pasivos_module.R" = "psv", "R/pasivos_table_module.R" = "psv", "R/pasivos_list_module.R" = "psv",
  "R/pasivos_wizard_module.R" = "psv", "R/pasivos_edit_confirm_module.R" = "psv",
  "R/requisiciones_module.R" = "req", "R/forecasting_module.R" = "fc", "R/reporte_module.R" = "rpt",
  "R/tiers_module.R" = "tiers", "R/audit_log_viewer_module.R" = "tiers", "R/pagos_audit_viewer_module.R" = "ph",
  "R/empresas_module.R" = "emp", "R/search_module.R" = "lupa", "R/cashflow_export_module.R" = "flujo",
  "R/proveedores_module.R" = "config.catalogo", "R/hd_table.R" = "tablas_motor")
.pb_slug <- function(x) {
  x <- iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "")
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_+|_+$", "", x)
}
pantallas_borrador <- function(b, d = NULL) {
  if (!is.null(d)) b <- b[!paste(b$archivo, b$ancla) %in% paste(d$archivo, d$ancla), , drop = FALSE]
  if (!nrow(b)) return(as.data.frame(setNames(replicate(length(PANTALLAS_COLUMNAS), character(), simplify = FALSE), PANTALLAS_COLUMNAS)))
  es <- tryCatch(jsonlite::fromJSON("www/i18n/es.json"), error = function(e) list())
  nombre <- vapply(b$etiqueta, function(x) {
    if (is.na(x)) return("")
    if (startsWith(x, "tr:")) return(es[[sub("^tr:", "", x)]] %||% x)
    x }, "", USE.NAMES = FALSE)
  raiz <- unname(.PB_RAIZ[b$archivo]); raiz[is.na(raiz)] <- ""
  valor <- sub("^[a-z]+:", "", b$ancla)
  tipo <- ifelse(b$clase == "nav", ifelse(b$archivo == "app.R", "pestana", "subpestana"),
          ifelse(b$clase == "descarga", "excel", b$clase))
  id <- ifelse(b$clase == "nav" & b$archivo == "app.R", tolower(valor),
        ifelse(b$clase %in% c("correo", "pagina"), paste0(b$clase, ".", .pb_slug(valor)),
               paste0(ifelse(nzchar(raiz), raiz, "app"), ".", .pb_slug(valor))))
  data.frame(id = id, nombre = nombre, tipo = tipo,
             padre = ifelse(tipo %in% c("pestana", "correo", "pagina"), "", ifelse(nzchar(raiz), raiz, "")),
             archivo = b$archivo, ancla = b$ancla, niveles = "", permiso = "", tablas = "",
             diseno = "pendiente", notas = "", stringsAsFactors = FALSE)
}

if (sys.nframe() == 0L && !interactive()) {
  args <- commandArgs(trailingOnly = TRUE)
  ruta <- "docs/diseno/pantallas.tsv"
  d <- if (file.exists(ruta)) pantallas_leer(ruta) else NULL
  nuevo <- pantallas_borrador(pantallas_barrer("."), d)
  cat(sprintf("%d fila(s) que el TSV no tiene\n", nrow(nuevo)))
  if ("--escribir" %in% args && nrow(nuevo)) {
    escribir_cab <- is.null(d)
    con <- file(ruta, open = if (escribir_cab) "w" else "a", encoding = "UTF-8")
    utils::write.table(nuevo, con, sep = "\t", quote = FALSE, row.names = FALSE, col.names = escribir_cab)
    close(con)
    cat("agregadas a", ruta, "-- revisa nombre, padre e id a mano\n")
  } else if (nrow(nuevo)) {
    utils::write.table(nuevo, stdout(), sep = "\t", quote = FALSE, row.names = FALSE)
  }
}
