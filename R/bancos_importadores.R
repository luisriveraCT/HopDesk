# =============================================================================
# R/bancos_importadores.R -- T84 A2: the bank-statement importers' registry
#
# Bancos (R/bancos_module.R, "Importar TXT") used to name BanBajío at five
# places: which accounts can take a file, how the file is read, the account
# detector and the parser (twice). It now asks this registry. One entry per
# file format a bank gives; BanBajío's TXT is the only one today. A second
# bank, or BanBajío's MT940 once there is a sample, is one more entry with its
# own golden test (tests/fixtures/t84/) -- nothing else changes.
#
# Functions are stored by NAME (as ERP_CONNECTOR_REGISTRY does), so this file
# can load before the ones that define them (R/bancos_parser.R,
# R/bancos_detect.R) and a test can swap one.
#   acepta_cuenta(cuentas)          logical per row: the accounts this
#                                   importer can load a file into
#   leer(path)                      the file's lines, or NULL if unreadable
#   detectar(lineas, cuentas, empresas, parsed)   the account ranking
#                                   (R/bancos_detect.R's contract)
#   parsear(lineas, cuenta_id)      canonical bancos_movimientos rows
#                                   (R/bancos_parser.R's contract)
# Out of A2 on purpose: dedup (it works on canonical rows) and the Libro's
# export format (BANCOS_FORMATOS_EXPORT, already a registry).
# tests/test_t84_importadores.R: the registry gives A1's golden rows, and no
# file but this one calls parse_banbajio_txt / detectar_cuenta_banbajio.
# =============================================================================

BANCOS_IMPORTADORES <- list(
  banbajio_txt = list(
    banco         = "BanBajío",
    formato       = "TXT de Consulta de Movimientos (BajíoNet)",
    acepta_cuenta = "importador_banbajio_acepta",
    leer          = "importador_leer_texto",
    detectar      = "detectar_cuenta_banbajio",
    parsear       = "parse_banbajio_txt",
    fuente        = "txt"
  )
)

# Active BanBajío accounts: the only ones a BajíoNet TXT can go into. As
# Bancos filtered them before (activa == TRUE and "baj" in the bank's name).
importador_banbajio_acepta <- function(cuentas) {
  if (is.null(cuentas) || !nrow(cuentas)) return(logical(0))
  cuentas$activa %in% TRUE & grepl("baj", cuentas$banco, ignore.case = TRUE)
}

# A statement file's lines: UTF-8 first, latin1 if that read fails; NULL when
# neither reads. Bancos had this twice.
importador_leer_texto <- function(path) {
  tryCatch(
    readLines(path, encoding = "UTF-8", warn = FALSE),
    error = function(e) tryCatch(readLines(path, encoding = "latin1", warn = FALSE),
                                 error = function(e2) NULL)
  )
}

# The importer for the client's accounts: the first entry that accepts one of
# them; with none (no account configured yet), the first entry -- the screen
# then says there is no account to import into, as before.
importador_de_cuenta <- function(cuentas = NULL) {
  for (nm in names(BANCOS_IMPORTADORES)) {
    f <- get(BANCOS_IMPORTADORES[[nm]]$acepta_cuenta, mode = "function")
    if (!is.null(cuentas) && nrow(cuentas) && any(f(cuentas))) return(nm)
  }
  names(BANCOS_IMPORTADORES)[1]
}

# Call one of an importer's functions: importador_llamar("banbajio_txt", "parsear", lineas, cuenta_id).
importador_llamar <- function(imp, que, ...) {
  e <- BANCOS_IMPORTADORES[[imp]]
  if (is.null(e)) stop("[importadores] importador desconocido: ", imp, call. = FALSE)
  fn <- e[[que]]
  if (is.null(fn)) stop("[importadores] ", imp, " no tiene ", que, call. = FALSE)
  do.call(get(fn, mode = "function"), list(...))
}
