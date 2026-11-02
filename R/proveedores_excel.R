# =============================================================================
# R/proveedores_excel.R
# The Proveedores Excel import, as pure functions: no Shiny, no S3. The module
# (R/proveedores_module.R) reads the file with prov_leer_excel(), classifies
# with prov_clasificar(), shows the result and applies it with prov_aplicar().
#
# Why this exists (T08): the old import lost data in ways nobody saw --
# Reemplazar deleted and recreated the provider (id, empresas, activo_hasta
# and every field the file left empty were gone), invisible spaces (no-break,
# thin, zero-width, BOM) stayed in CLABEs and RFCs, headers with accents or
# dots weren't found, a CLABE Excel kept as a number came back with other
# digits, and bank 30 paid as SPI instead of BCO. Every rule below answers one
# of those (tests/test_proveedores_excel_repro.R).
# =============================================================================

# ── Cells ────────────────────────────────────────────────────────────────────
# Every Unicode space, plus zero-width ones and the BOM.
.PROV_ESPACIO <- "[\\s\\p{Z}\\u200B-\\u200D\\u2060\\uFEFF]"

# Trims every kind of space at both ends; "", "NA", "N/A" and "-" are empty.
prov_limpiar_texto <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_replace_all_regex(x, paste0("^", .PROV_ESPACIO, "+|", .PROV_ESPACIO, "+$"), "")
  # Spaces inside become one plain space ("FLETES  EL GUERO").
  x <- stringi::stri_replace_all_regex(x, paste0(.PROV_ESPACIO, "+"), " ")
  x[is.na(x) | toupper(x) %in% c("", "NA", "N/A", "-")] <- NA_character_
  x
}

# One field's cleaning: the account loses every space and hyphen, the RFC
# goes upper case without spaces, the e-mail lower case.
prov_limpiar_campo <- function(campo, x) {
  x <- prov_limpiar_texto(x)
  switch(campo,
    no_cuenta = stringi::stri_replace_all_regex(x, paste0(.PROV_ESPACIO, "|-"), ""),
    rfc       = toupper(stringi::stri_replace_all_regex(x, .PROV_ESPACIO, "")),
    correo    = tolower(stringi::stri_replace_all_regex(x, paste0(.PROV_ESPACIO, "*,", .PROV_ESPACIO, "*"), ",")),
    x)
}

# ── Headers ──────────────────────────────────────────────────────────────────
# Clean, without accents, lower case, punctuation as space: "Núm. de cuenta"
# -> "num de cuenta", "R.F.C." -> "r f c", "no_cuenta" -> "no cuenta".
prov_normalizar_encabezado <- function(x) {
  x <- prov_limpiar_texto(x)
  x <- tolower(stringi::stri_trans_general(x, "Latin-ASCII"))
  x <- stringi::stri_replace_all_regex(x, "[^a-z0-9]+", " ")
  trimws(x)
}

# What each field's header may say, already normalized. Exact match first;
# then, for what's still missing, a header containing one of the words.
PROV_EXCEL_CAMPOS <- list(
  nombre        = c("nombre", "nombre del beneficiario", "beneficiario", "nombre del proveedor", "proveedor",
                    "razon social", "nombre beneficiario"),
  alias         = c("alias", "alias del beneficiario", "alias beneficiario", "alias corto"),
  banco         = c("banco", "banco destino", "clave banco", "clave del banco", "banco beneficiario"),
  tipo_cuenta   = c("tipo de cuenta", "tipo cuenta"),
  no_cuenta     = c("no cuenta", "no de cuenta", "num de cuenta", "numero de cuenta", "num cuenta", "cuenta",
                    "clabe", "cuenta clabe", "no cuenta clabe", "clabe interbancaria", "no de cuenta clabe"),
  rfc           = c("rfc", "r f c"),
  correo        = c("correo", "correo electronico", "email", "e mail", "mail"),
  moneda        = c("moneda", "divisa"),
  tipo_relacion = c("tipo de relacion", "tipo relacion", "relacion")
)
# Word searched in the second pass, in an order where the specific goes before
# the generic ("Tipo de cuenta" is not the account; "Nombre del banco" is the bank).
.PROV_EXCEL_CONTIENE <- list(
  tipo_cuenta = "tipo( de)? cuenta", tipo_relacion = "relacion", banco = "banco",
  no_cuenta = c("clabe", "cuenta"), rfc = "rfc", correo = c("correo", "e ?mail"), moneda = c("moneda", "divisa"),
  alias = "alias", nombre = c("nombre", "beneficiario", "proveedor", "razon social")
)

# Field -> the file's header it reads from (NULL when not found).
prov_mapear_columnas <- function(encabezados) {
  norm  <- prov_normalizar_encabezado(encabezados)
  norm[is.na(norm)] <- ""
  usado <- logical(length(norm))
  out   <- stats::setNames(vector("list", length(PROV_EXCEL_CAMPOS)), names(PROV_EXCEL_CAMPOS))
  for (campo in names(PROV_EXCEL_CAMPOS)) {
    j <- which(!usado & norm %in% PROV_EXCEL_CAMPOS[[campo]])[1]
    if (!is.na(j)) { out[[campo]] <- encabezados[j]; usado[j] <- TRUE }
  }
  for (campo in names(.PROV_EXCEL_CONTIENE)) {
    if (!is.null(out[[campo]])) next
    pat <- paste0("\\b(", paste(.PROV_EXCEL_CONTIENE[[campo]], collapse = "|"), ")\\b")
    j <- which(!usado & nzchar(norm) & grepl(pat, norm, perl = TRUE))[1]
    if (!is.na(j)) { out[[campo]] <- encabezados[j]; usado[j] <- TRUE }
  }
  out
}

# Format B (header row) when the first row has 2 or more known headers, by
# exact name: a data row rarely says just "Banco" or "RFC" in a cell.
prov_tiene_encabezado <- function(fila1) {
  norm <- prov_normalizar_encabezado(fila1)
  sum(norm %in% unlist(PROV_EXCEL_CAMPOS), na.rm = TRUE) >= 2L
}

# The BanBajío file without header: columns by position.
PROV_EXCEL_POSICION <- c("nombre", "alias", "banco", "tipo_cuenta", "no_cuenta", "rfc", "correo", "moneda",
                         "tipo_relacion")

# ── Reading ──────────────────────────────────────────────────────────────────
# A cell as text, and whether Excel kept it as a number. Whole numbers are
# written without decimals or exponent; what Excel already rounded can't be
# undone here -- prov_cuenta() decides whether to trust it.
.prov_celda <- function(v) {
  if (is.null(v) || length(v) == 0L || (length(v) == 1L && is.na(v))) return(list(texto = NA_character_, numero = FALSE))
  if (is.numeric(v)) {
    txt <- if (is.finite(v) && v == round(v) && abs(v) < 1e21) sprintf("%.0f", v) else format(v, digits = 15)
    return(list(texto = txt, numero = TRUE))
  }
  if (inherits(v, c("POSIXt", "Date"))) return(list(texto = format(as.Date(v)), numero = FALSE))
  list(texto = as.character(v), numero = FALSE)
}

# Reads the file. Returns list(filas, formato = "A"/"B", columnas, faltan):
#   filas    one row per non-empty Excel row: .row_num, each field's raw text,
#            and .num_no_cuenta / .num_banco (the cell was a number in Excel)
#   columnas field -> where it came from ("No. Cuenta", or "columna E" in A)
#   faltan   fields the file doesn't have
# NULL when the file has no rows or can't be read.
prov_leer_excel <- function(path) {
  crudo <- tryCatch(suppressMessages(readxl::read_excel(path, col_names = FALSE, col_types = "list",
                                                        .name_repair = "minimal")),
                    error = function(e) NULL)
  if (is.null(crudo) || !nrow(crudo) || !ncol(crudo)) return(NULL)
  celdas <- lapply(crudo, function(col) lapply(col, .prov_celda))
  texto  <- vapply(celdas, function(col) vapply(col, `[[`, "", "texto"), character(nrow(crudo)))
  numero <- vapply(celdas, function(col) vapply(col, `[[`, TRUE, "numero"), logical(nrow(crudo)))
  if (is.null(dim(texto))) { texto <- matrix(texto, nrow = 1L); numero <- matrix(numero, nrow = 1L) }

  formato_b <- prov_tiene_encabezado(texto[1, ])
  if (formato_b) {
    mapa  <- prov_mapear_columnas(texto[1, ])
    datos <- seq_len(nrow(texto))[-1]
    col_de <- function(campo) if (is.null(mapa[[campo]])) NA_integer_ else match(mapa[[campo]], texto[1, ])
    columnas <- lapply(mapa, function(h) if (is.null(h)) NULL else prov_limpiar_texto(h))
  } else {
    datos  <- seq_len(nrow(texto))
    col_de <- function(campo) { j <- match(campo, PROV_EXCEL_POSICION); if (j <= ncol(texto)) j else NA_integer_ }
    columnas <- stats::setNames(lapply(PROV_EXCEL_POSICION, function(campo) {
      j <- col_de(campo); if (is.na(j)) NULL else paste("columna", LETTERS[j])
    }), PROV_EXCEL_POSICION)
  }
  campo_txt <- function(campo) { j <- col_de(campo); if (is.na(j)) rep(NA_character_, length(datos)) else texto[datos, j] }
  campo_num <- function(campo) { j <- col_de(campo); if (is.na(j)) rep(FALSE, length(datos)) else numero[datos, j] }
  filas <- data.frame(.row_num = datos, stringsAsFactors = FALSE)
  for (campo in names(PROV_EXCEL_CAMPOS)) filas[[campo]] <- campo_txt(campo)
  filas$.num_no_cuenta <- campo_num("no_cuenta")
  filas$.num_banco     <- campo_num("banco")
  # Rows with nothing that identifies a provider are trailing blanks.
  vacia <- Reduce(`&`, lapply(c("nombre", "no_cuenta", "banco", "rfc"), function(k) is.na(prov_limpiar_texto(filas[[k]]))))
  filas <- filas[!vacia, , drop = FALSE]
  rownames(filas) <- NULL
  faltan <- names(columnas)[vapply(columnas, is.null, logical(1))]
  list(filas = filas, formato = if (formato_b) "B" else "A", columnas = columnas[!vapply(columnas, is.null, logical(1))],
       faltan = faltan)
}

# ── Accounts ─────────────────────────────────────────────────────────────────
# CLABE check digit: weights 3, 7, 1 over the first 17 digits, modulo 10.
prov_clabe_valida <- function(x) {
  x <- as.character(x)
  vapply(x, function(s) {
    if (is.na(s) || !grepl("^[0-9]{18}$", s)) return(FALSE)
    d <- as.integer(strsplit(s, "", fixed = TRUE)[[1]])
    (10L - sum((d[1:17] * rep(c(3L, 7L, 1L), 6L)[1:17]) %% 10L) %% 10L) %% 10L == d[18]
  }, logical(1), USE.NAMES = FALSE)
}

# Largest whole number a double keeps exactly: above it, Excel already
# changed digits and nothing can tell which.
.PROV_ENTERO_EXACTO <- 2^53

# One account: list(valor, estado = "ok" | "reparada" | "revisar", motivo).
# motivo is a dictionary key (tablas.proveedores.importar.*).
prov_cuenta <- function(texto, es_numero = FALSE) {
  t <- prov_limpiar_campo("no_cuenta", texto)
  if (is.na(t)) return(list(valor = NA_character_, estado = "ok", motivo = NA_character_))
  revisar <- function(m) list(valor = t, estado = "revisar", motivo = m)
  if (grepl("^[0-9.]+[eE][+-]?[0-9]+$", t) || (isTRUE(es_numero) && grepl("[.]", t)))
    return(revisar("tablas.proveedores.importar.motivo_numero"))
  if (isTRUE(es_numero) && grepl("^[0-9]+$", t) && as.numeric(t) > .PROV_ENTERO_EXACTO)
    return(revisar("tablas.proveedores.importar.motivo_numero"))
  if (!grepl("^[0-9]+$", t)) return(list(valor = t, estado = "ok", motivo = NA_character_))  # not a CLABE (abroad, etc.)
  n <- nchar(t)
  if (n == 18L) {
    if (prov_clabe_valida(t)) return(list(valor = t, estado = "ok", motivo = NA_character_))
    return(revisar("tablas.proveedores.importar.motivo_dv"))
  }
  # Excel drops leading zeros of a number (a text cell of 17 digits too, when
  # someone typed it without the 0): put them back only if the check digit
  # proves it.
  if (n == 17L || (isTRUE(es_numero) && n == 16L)) {
    con_ceros <- paste0(strrep("0", 18L - n), t)
    if (prov_clabe_valida(con_ceros))
      return(list(valor = con_ceros, estado = "reparada", motivo = "tablas.proveedores.importar.motivo_cero"))
    return(revisar("tablas.proveedores.importar.motivo_17"))
  }
  list(valor = t, estado = "ok", motivo = NA_character_)          # 10-12 or 16 digits: not a CLABE
}

# ── Bank ─────────────────────────────────────────────────────────────────────
# The 3-digit key, as the edit modal stores it: "30" -> "030",
# "012-BBVA BANCOMER" -> "012", "Banorte" -> "072" (by the catalog's names).
# list(valor, estado = "ok" | "revisar", motivo).
prov_banco_clave <- function(texto, catalogo = character(0)) {
  t <- prov_limpiar_texto(texto)
  if (is.na(t)) return(list(valor = NA_character_, estado = "ok", motivo = NA_character_))
  ok <- function(v) list(valor = v, estado = "ok", motivo = NA_character_)
  if (grepl("^[0-9]{1,3}$", t)) return(ok(formatC(as.integer(t), width = 3L, flag = "0")))
  if (grepl("^[0-9]{3}([^0-9]|$)", t)) return(ok(substr(t, 1L, 3L)))
  if (length(catalogo)) {
    partes <- strsplit(catalogo[nzchar(catalogo) & grepl("\\|", catalogo)], "|", fixed = TRUE)
    partes <- Filter(function(p) length(p) == 2L && grepl("^[0-9]{3}$", p[2]), partes)
    nombres <- prov_normalizar_encabezado(vapply(partes, `[[`, "", 1L))
    claves  <- unname(vapply(partes, `[[`, "", 2L))
    j <- match(prov_normalizar_encabezado(t), nombres)
    if (!is.na(j)) return(ok(claves[j]))
  }
  list(valor = t, estado = "revisar", motivo = "tablas.proveedores.importar.motivo_banco")
}

# ── Currency, payment type, status ───────────────────────────────────────────
prov_moneda <- function(x) {
  t <- prov_normalizar_encabezado(x)
  out <- prov_limpiar_texto(x)
  out[!is.na(t) & grepl("^(mxn|mn|m n|peso|pesos|moneda nacional)$", t)] <- "MXN"
  out[!is.na(t) & grepl("^(usd|us|dll|dlls|dolar|dolares|dolar americano)$", t)] <- "USD"
  out
}

# As the edit modal: USD pays SPD, BanBajío (030) BCO, the rest SPI.
prov_medio_pago <- function(moneda, banco) {
  ifelse(!is.na(moneda) & moneda == "USD", "SPD", ifelse(!is.na(banco) & banco == "030", "BCO", "SPI"))
}

prov_status <- function(nombre, moneda, banco, no_cuenta) {
  lleno <- function(v) !is.na(v) & nzchar(trimws(v))
  ifelse(lleno(nombre) & lleno(moneda) & lleno(banco) & lleno(no_cuenta), "completo", "incompleto")
}

# ── Classifying ──────────────────────────────────────────────────────────────
# The fields the Excel can bring, in the order the review shows changes.
PROV_EXCEL_ACTUALIZA <- c("nombre", "alias", "banco", "no_cuenta", "rfc", "correo", "moneda", "tipo_relacion",
                          "tipo_cuenta")

# An existing provider's account as the import compares it: no_cuenta, else
# clabe, row by row (25 providers in dev only have it in clabe).
prov_cuenta_existente <- function(df) {
  if (is.null(df) || !nrow(df)) return(character(0))
  nc <- if ("no_cuenta" %in% names(df)) as.character(df$no_cuenta) else rep(NA_character_, nrow(df))
  cl <- if ("clabe" %in% names(df)) as.character(df$clabe) else rep(NA_character_, nrow(df))
  prov_limpiar_campo("no_cuenta", dplyr::coalesce(prov_limpiar_texto(nc), prov_limpiar_texto(cl)))
}

# A stored value as the import compares it (so a legacy "012-BBVA BANCOMER"
# or "Pesos" doesn't count as a change against "012" or "MXN").
.prov_comparable <- function(campo, x, catalogo) {
  switch(campo,
    banco     = vapply(x, function(v) prov_banco_clave(v, catalogo)$valor, "", USE.NAMES = FALSE),
    moneda    = prov_moneda(x),
    no_cuenta = prov_limpiar_campo("no_cuenta", x),
    prov_limpiar_campo(campo, x))
}

# Cleans the read rows and sorts each one:
#   listo        new, ready
#   incompleto   no name, account or bank
#   revisar      account or bank that can't be trusted (.motivo says why);
#                never imported until the file is fixed
#   duplicado    the same account earlier in the file
#   actualiza    an existing provider of the list it goes to, with changes
#   sin_cambios  an existing provider, nothing to change
#   reactiva     an existing INACTIVE provider, importing into Activos (or
#   inactiva     the reverse): updated in place and moved
# Adds .cambios (list column of list(campo, antes, despues)), .nota (the
# repair note or the alias warning), .existente_id and .existente_lista
# ("activos"/"inactivos"), .alias_vinculos (Partes linked to the old alias).
# tipo: the list the file goes to ("activos" / "inactivos").
prov_clasificar <- function(filas, activos, inactivos, tipo = "activos", catalogo = character(0),
                            alias_mapa = NULL) {
  n <- nrow(filas)
  f <- filas
  for (campo in setdiff(names(PROV_EXCEL_CAMPOS), c("no_cuenta", "banco", "moneda")))
    f[[campo]] <- prov_limpiar_campo(campo, f[[campo]])
  cuentas <- lapply(seq_len(n), function(i) prov_cuenta(f$no_cuenta[i], isTRUE(f$.num_no_cuenta[i])))
  bancos  <- lapply(seq_len(n), function(i) prov_banco_clave(f$banco[i], catalogo))
  f$no_cuenta <- vapply(cuentas, `[[`, "", "valor")
  f$banco     <- vapply(bancos, `[[`, "", "valor")
  f$moneda    <- prov_moneda(f$moneda)
  motivo <- ifelse(vapply(cuentas, `[[`, "", "estado") == "revisar", vapply(cuentas, `[[`, "", "motivo"),
            ifelse(vapply(bancos, `[[`, "", "estado") == "revisar", vapply(bancos, `[[`, "", "motivo"), NA_character_))
  f$.motivo <- motivo
  f$.nota   <- ifelse(vapply(cuentas, `[[`, "", "estado") == "reparada", vapply(cuentas, `[[`, "", "motivo"), NA_character_)

  llave_act <- prov_cuenta_existente(activos)
  llave_ina <- prov_cuenta_existente(inactivos)
  f$.existente_id    <- NA_character_
  f$.existente_lista <- NA_character_
  f$.cambios         <- rep(list(list()), n)
  f$.alias_vinculos  <- 0L
  estado <- character(n)
  vistas <- character(0)
  for (i in seq_len(n)) {
    if (!is.na(f$.motivo[i])) { estado[i] <- "revisar"; next }
    if (is.na(f$nombre[i]) || is.na(f$no_cuenta[i]) || is.na(f$banco[i])) { estado[i] <- "incompleto"; next }
    k <- f$no_cuenta[i]
    # The same account earlier in the file: a duplicate, whether it is new or
    # already in the catalog (applying it twice would be the second's word).
    if (k %in% vistas) { estado[i] <- "duplicado"; next }
    vistas <- c(vistas, k)
    j_act <- match(k, llave_act); j_ina <- match(k, llave_ina)
    if (!is.na(j_act) || !is.na(j_ina)) {
      lista <- if (!is.na(j_act)) "activos" else "inactivos"
      ex <- if (!is.na(j_act)) activos[j_act, , drop = FALSE] else inactivos[j_ina, , drop = FALSE]
      f$.existente_id[i] <- as.character(ex$id)
      f$.existente_lista[i] <- lista
      cambios <- list()
      for (campo in PROV_EXCEL_ACTUALIZA) {
        nuevo <- f[[campo]][i]
        if (is.na(nuevo)) next                                  # empty or missing column: keep
        antes <- if (campo == "no_cuenta") prov_cuenta_existente(ex) else if (campo %in% names(ex)) as.character(ex[[campo]]) else NA_character_
        if (identical(.prov_comparable(campo, antes, catalogo), nuevo)) next
        cambios[[length(cambios) + 1L]] <- list(campo = campo, antes = antes, despues = nuevo)
      }
      f$.cambios[[i]] <- cambios
      # A new alias breaks the Parte -> alias links (parte_alias_map): the PPL
      # would stop finding this provider. Flag it; the map isn't touched here.
      viejo <- prov_limpiar_texto(ex$alias)
      if (!is.na(f$alias[i]) && !is.na(viejo) && !identical(toupper(f$alias[i]), toupper(viejo)) &&
          !is.null(alias_mapa) && nrow(alias_mapa))
        f$.alias_vinculos[i] <- sum(toupper(prov_limpiar_texto(alias_mapa$alias)) == toupper(viejo), na.rm = TRUE)
      estado[i] <- if (lista != tipo) { if (tipo == "activos") "reactiva" else "inactiva" }
                   else if (length(cambios)) "actualiza" else "sin_cambios"
      next
    }
    estado[i] <- "listo"
  }
  f$.estado <- estado
  f
}

# ── Applying ─────────────────────────────────────────────────────────────────
# What Importar writes. clasificadas: prov_clasificar()'s rows; decisiones:
# named list account -> "reemplazar" | "conservar" | "omitir" for the rows
# that touch an existing provider (actualiza / reactiva / inactiva; nothing
# else). opciones: list(tipo, empresa, moneda (the file's, for new rows
# without one), incluir_incompletos, incluir_duplicados, cambiar_alias,
# lote, ahora, nuevo_id = function() <uuid>).
# Returns list(activos, inactivos, cambia_activos, cambia_inactivos,
# nuevos, actualizados, movidos, omitidos_alias).
prov_aplicar <- function(clasificadas, activos, inactivos, decisiones = list(), opciones = list()) {
  op <- utils::modifyList(list(tipo = "activos", empresa = "", moneda = "MXN", incluir_incompletos = FALSE,
                               incluir_duplicados = FALSE, cambiar_alias = FALSE, lote = NA_character_,
                               ahora = as.character(Sys.time()), nuevo_id = function() uuid::UUIDgenerate()),
                          opciones)
  f <- clasificadas
  out <- list(activos = activos, inactivos = inactivos, cambia_activos = FALSE, cambia_inactivos = FALSE,
              nuevos = 0L, actualizados = 0L, movidos = 0L, omitidos_alias = 0L)
  destino <- function(lista) if (lista == "activos") "activos" else "inactivos"
  tipo_lista <- destino(op$tipo)

  # New providers: ready rows, and the opted-in incomplete / first duplicate.
  vistos <- character(0)
  nuevas <- integer(0)
  for (i in seq_len(nrow(f))) {
    e <- f$.estado[i]
    if (e == "listo" || (e == "incompleto" && isTRUE(op$incluir_incompletos))) { nuevas <- c(nuevas, i); next }
    if (e == "duplicado" && isTRUE(op$incluir_duplicados) && !f$no_cuenta[i] %in% vistos) {
      nuevas <- c(nuevas, i); vistos <- c(vistos, f$no_cuenta[i])
    }
  }
  if (length(nuevas)) {
    x <- f[nuevas, , drop = FALSE]
    moneda <- dplyr::coalesce(x$moneda, op$moneda)
    filas_nuevas <- data.frame(
      id = vapply(seq_along(nuevas), function(k) op$nuevo_id(), ""), Empresa = op$empresa, codigo = NA_character_,
      nombre = x$nombre, alias = x$alias, clabe = x$no_cuenta, medio_pago = prov_medio_pago(moneda, x$banco),
      rfc = x$rfc, tipo = "012", banco_destino = x$banco, activo = op$tipo == "activos", no_cuenta = x$no_cuenta,
      banco = x$banco, tipo_cuenta = x$tipo_cuenta, correo = x$correo, moneda = moneda,
      tipo_relacion = x$tipo_relacion, empresas = op$empresa,
      status = prov_status(x$nombre, moneda, x$banco, x$no_cuenta), fuente = "excel", upload_batch = op$lote,
      activo_hasta = "indefinido", created_at = op$ahora, updated_at = op$ahora, stringsAsFactors = FALSE)
    out[[tipo_lista]] <- dplyr::bind_rows(out[[tipo_lista]], filas_nuevas)
    out[[paste0("cambia_", tipo_lista)]] <- TRUE
    out$nuevos <- length(nuevas)
  }

  # Existing providers: update in place -- only what the file brings and
  # differs -- and move them when the file goes to the other list.
  for (i in which(f$.estado %in% c("actualiza", "reactiva", "inactiva"))) {
    if (!identical(decisiones[[f$no_cuenta[i]]], "reemplazar")) next
    if (f$.alias_vinculos[i] > 0L && !isTRUE(op$cambiar_alias)) { out$omitidos_alias <- out$omitidos_alias + 1L; next }
    origen <- f$.existente_lista[i]
    j <- match(f$.existente_id[i], as.character(out[[origen]]$id))
    if (is.na(j)) next
    for (c in f$.cambios[[i]])                                  # a field the list never had (tipo_cuenta)
      if (!c$campo %in% names(out[[origen]])) out[[origen]][[c$campo]] <- NA_character_
    fila <- out[[origen]][j, , drop = FALSE]
    for (c in f$.cambios[[i]]) {
      fila[[c$campo]] <- c$despues
      if (c$campo == "no_cuenta") fila$clabe <- c$despues
      if (c$campo == "banco") fila$banco_destino <- c$despues
    }
    moneda <- prov_moneda(fila$moneda)
    fila$medio_pago <- prov_medio_pago(moneda, prov_banco_clave(fila$banco)$valor)
    fila$status     <- prov_status(fila$nombre, moneda, fila$banco, dplyr::coalesce(fila$no_cuenta, fila$clabe))
    fila$updated_at <- op$ahora
    if (origen == tipo_lista) {
      out[[origen]][j, ] <- fila
      out[[paste0("cambia_", origen)]] <- TRUE
      out$actualizados <- out$actualizados + 1L
    } else {
      fila$activo <- tipo_lista == "activos"
      out[[origen]] <- out[[origen]][-j, , drop = FALSE]
      out[[tipo_lista]] <- dplyr::bind_rows(out[[tipo_lista]], fila)
      out$cambia_activos <- TRUE; out$cambia_inactivos <- TRUE
      out$movidos <- out$movidos + 1L
    }
  }
  out
}

# ── Template ─────────────────────────────────────────────────────────────────
# The round-trip file: every column the import reads, with the headers it
# recognizes, and the account, RFC and bank as TEXT cells so Excel doesn't
# turn them into numbers while someone edits them.
PROV_PLANTILLA_COLUMNAS <- c(nombre = "Nombre", alias = "Alias", banco = "Banco", tipo_cuenta = "Tipo de cuenta",
                             no_cuenta = "No. Cuenta", rfc = "RFC", correo = "Correo", moneda = "Moneda",
                             tipo_relacion = "Tipo de relación")

prov_plantilla_datos <- function(df, catalogo = character(0)) {
  if (is.null(df) || !nrow(df)) df <- data.frame(nombre = character(0))
  col <- function(k) if (k %in% names(df)) as.character(df[[k]]) else rep(NA_character_, nrow(df))
  out <- data.frame(
    nombre = col("nombre"), alias = col("alias"),
    banco = vapply(col("banco"), function(v) { b <- prov_banco_clave(v, catalogo); if (b$estado == "ok") b$valor else v },
                   "", USE.NAMES = FALSE),
    tipo_cuenta = col("tipo_cuenta"), no_cuenta = prov_cuenta_existente(df), rfc = col("rfc"),
    correo = col("correo"), moneda = col("moneda"), tipo_relacion = col("tipo_relacion"), stringsAsFactors = FALSE)
  out[is.na(out)] <- ""
  stats::setNames(out, PROV_PLANTILLA_COLUMNAS[names(out)])
}

prov_plantilla_escribir <- function(df, archivo, catalogo = character(0), filas_extra = 500L) {
  datos <- prov_plantilla_datos(df, catalogo)
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Proveedores")
  texto <- openxlsx::createStyle(numFmt = "@")
  cols_texto <- match(c("Banco", "No. Cuenta", "RFC"), names(datos))
  # Text format before the values (and on spare rows below), so typing there
  # stays text too.
  openxlsx::addStyle(wb, "Proveedores", texto, rows = 2:(nrow(datos) + 1L + filas_extra),
                     cols = cols_texto, gridExpand = TRUE)
  openxlsx::writeData(wb, "Proveedores", datos)
  openxlsx::addStyle(wb, "Proveedores", openxlsx::createStyle(textDecoration = "bold"), rows = 1, cols = seq_along(datos))
  openxlsx::setColWidths(wb, "Proveedores", cols = seq_along(datos), widths = "auto")
  openxlsx::saveWorkbook(wb, archivo, overwrite = TRUE)
  invisible(archivo)
}
