# =============================================================================
# R/pagos_layouts.R -- T84 B2: the payment-file layouts' registry
#
# The Agenda (R/pagar_hoy_module.R) wrote BanBajío's PPL by name at its three
# downloads (the per-company file, the ZIP's files and the ZIP's individual
# _002 files). It now asks this registry. One entry per bank layout; BanBajío's
# "Pago a Proveedores" is the only one today. Another bank's layout is one
# more entry with its own byte-by-byte golden file (tests/fixtures/t84/).
#
# Functions by NAME (as ERP_CONNECTOR_REGISTRY and BANCOS_IMPORTADORES):
#   validar, generar, escribir      R/ppl_generator.R's contracts (generar
#                                   keeps generate_ppl()'s signature, T94's
#                                   moneda and clasificacion included)
#   moneda_ok, medio_pago           the layout's currency rules
#   leer_respuesta                  the bank's answer about the file we sent
#                                   (T93, R/bancos_respuesta.R) -- listed here
#                                   for reference only; nothing calls it
#                                   through the registry yet
# tests/test_t84_layouts.R: through the registry the four frozen PPL files
# come out byte for byte (T84 B1), and no file but this one calls
# generate_ppl() / write_ppl().
# =============================================================================

PAGOS_LAYOUTS <- list(
  banbajio_ppl = list(
    banco          = "BanBajío",
    formato        = "PPL pago a proveedores (384 car.)",
    validar        = "validate_ppl",
    generar        = "generate_ppl",
    escribir       = "write_ppl",
    moneda_ok      = "ppl_moneda_soportada",
    medio_pago     = "ppl_medio_pago",
    leer_respuesta = "leer_transferencias_bajionet"
  )
)

# The layout for a payment's origin account. Every origin account pays
# through BanBajío today; a second bank chooses here (by the account's bank).
layout_de_cuenta <- function(cuenta_origen = NULL) names(PAGOS_LAYOUTS)[1]

# Call one of a layout's functions: layout_llamar("banbajio_ppl", "generar", pagos, origen, fecha, 1L, ...).
layout_llamar <- function(lay, que, ...) {
  e <- PAGOS_LAYOUTS[[lay]]
  if (is.null(e)) stop("[layouts] layout desconocido: ", lay, call. = FALSE)
  fn <- e[[que]]
  if (is.null(fn)) stop("[layouts] ", lay, " no tiene ", que, call. = FALSE)
  do.call(get(fn, mode = "function"), list(...))
}
