# =============================================================================
# R/monedas.R
# The one place that knows currencies (Luis, 2026-11-16: "cero hardcoding";
# monedas fase 1, requisiciones/flujo_monedas_paso1.md).
#
#   - The catalog is ISO 4217 (list 1, legal tender only), versioned in
#     data/monedas_iso4217.csv. Nothing else in the app should spell a
#     currency code: tests/test_monedas_guardian.R counts what is left and
#     never lets the count grow (it fails at zero's ratchet, phase 3).
#   - The client's base (presentation) currency lives in its group config
#     (Ajustes generales, can_manage_group_branding). Until someone sets
#     it, MONEDA_BASE_OMISION (Luis), and Ajustes says so.
#   - The other currencies come from the data, base first.
#
# Pure functions: the setting is saved with the rest of the group config
# (R/empresas_module.R, "Guardar configuración").
# =============================================================================

MONEDAS_VERSION <- "2026-11-16b"
.MONEDAS_ARCHIVO <- "data/monedas_iso4217.csv"

.monedas_leer <- function(ruta = .MONEDAS_ARCHIVO) {
  df <- utils::read.csv(ruta, comment.char = "#", colClasses = "character",
                        encoding = "UTF-8", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  df$decimales <- as.integer(df$decimales)
  df
}
.MONEDAS <- .monedas_leer()

# Every ISO code in the catalog, alphabetical.
monedas_catalogo <- function() .MONEDAS$codigo

# TRUE for each element that is a catalog code (exactly, upper case).
moneda_valida <- function(x) {
  x <- as.character(x)
  !is.na(x) & x %in% .MONEDAS$codigo
}

# Minor units of each code (NA for one outside the catalog).
moneda_decimales <- function(x) .MONEDAS$decimales[match(as.character(x), .MONEDAS$codigo)]

# The currency's name in `idioma` ("es" | "en"); the code itself when it is
# not in the catalog, so a strange value is shown, never hidden.
moneda_nombre <- function(x, idioma = "es") {
  col <- if (identical(idioma, "en")) "nombre_en" else "nombre_es"
  n <- .MONEDAS[[col]][match(as.character(x), .MONEDAS$codigo)]
  ifelse(is.na(n), as.character(x), n)
}

# Choices for a selector: "MXN — Peso mexicano" = "MXN". `primero` go on
# top in that order (the base, then the data's), the rest alphabetical.
monedas_opciones <- function(primero = character(), idioma = "es") {
  primero <- unique(primero[moneda_valida(primero)])
  cods <- c(primero, setdiff(.MONEDAS$codigo, primero))
  stats::setNames(cods, paste0(cods, " — ", moneda_nombre(cods, idioma)))
}

# ── Monedas en los datos ─────────────────────────────────────────────────────
# Currencies that appear in `monedas` (one per document row), ordered by how
# many documents use each, ties alphabetical. Upper-cased and trimmed; empty
# and NA dropped. Codes outside the catalog are kept (data is data), last.
.monedas_por_documentos <- function(monedas) {
  m <- toupper(trimws(as.character(monedas)))
  m <- m[!is.na(m) & nzchar(m)]
  if (!length(m)) return(character())
  t <- table(m)
  cods <- names(t)
  o <- order(!moneda_valida(cods), -as.integer(t), cods)
  cods[o]
}

# The base first, then the data's currencies by number of documents.
monedas_en_datos <- function(monedas, base = NA_character_) {
  d <- .monedas_por_documentos(monedas)
  if (!is.na(base) && moneda_valida(base)) c(base, setdiff(d, base)) else d
}

# ── Moneda base del cliente ──────────────────────────────────────────────────
# The default base currency when a client has not chosen one. Luis,
# 2026-11-16: "es una app mexicana". The one currency code written by hand
# in the app's logic; it lives here, in the catalog's file, which
# tests/test_monedas_guardian.R does not count, for this reason.
MONEDA_BASE_OMISION <- "MXN"

# list(moneda, origen):
#   origen "ajuste"   the client chose it (group config, a catalog code);
#          "omision"  nobody chose yet (or the value is not in the catalog):
#                     MONEDA_BASE_OMISION, and Ajustes says so.
moneda_base_resolver <- function(ajuste) {
  a <- toupper(trimws(as.character(ajuste %||% NA_character_)))[1]
  if (!is.na(a) && moneda_valida(a)) return(list(moneda = a, origen = "ajuste"))
  list(moneda = MONEDA_BASE_OMISION, origen = "omision")
}

# The `Moneda` column of every open document in sap_data() (AR and AP).
monedas_de_sap <- function(sap) {
  if (!is.list(sap)) return(character())
  unlist(lapply(sap[c("AR", "AP")], function(df)
    if (is.data.frame(df) && "Moneda" %in% names(df)) as.character(df$Moneda) else character()),
    use.names = FALSE)
}
