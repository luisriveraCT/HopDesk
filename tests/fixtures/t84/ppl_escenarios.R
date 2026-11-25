# T84 B1: the PPL scenarios whose files are frozen next to this one
# (ppl_<name>.txt). INVENTED data only: origin account, CLABEs, names and
# concepts are fictitious, never a real supplier or account. ASCII only, so
# the frozen bytes do not depend on the platform's iconv (accents are checked
# apart, in tests/test_t84_ppl_dorado.R, without freezing their bytes).
# Each scenario: list(pagos, cuenta_origen, fecha, consecutivo, moneda, clasificacion).
pago <- function(alias, clabe_dest, banco_dest, medio_pago, importe, concepto)
  data.frame(alias = alias, clabe_dest = clabe_dest, banco_dest = banco_dest, medio_pago = medio_pago,
             importe = importe, concepto = concepto, stringsAsFactors = FALSE)
FECHA_PPL <- as.Date("2026-11-27")
ORIGEN_PPL <- "012345678901"
list(
  # MXN: SPEI to two banks of the catalog (BBVA 012, Banamex 002) and an
  # internal BanBajío transfer (BCO, bank 030), with centavos and a long concept.
  mxn_mixto = list(
    pagos = rbind(
      pago("PROVEEDOR FICT UNO", "012180001234567891", "012", "SPI", 15234.56, "FACTURA F-0001 SERVICIOS FICTICIOS DE PRUEBA"),
      pago("PROV FICTICIO DOS", "002180009876543210", "002", "SPI", 0.01, "PAGO MINIMO"),
      pago("BAJIO FICTICIO", "000000000123456789", "030", "BCO", 1000000, strrep("CONCEPTO LARGO ", 20))),
    cuenta_origen = ORIGEN_PPL, fecha = FECHA_PPL, consecutivo = 1L, moneda = "MXN", clasificacion = NULL),
  # USD by SPID with the operation's classification (field 17).
  usd_spid = list(
    pagos = rbind(
      pago("SUPPLIER FICT INC", "012180001111222233", "012", "SPD", 2500.5, "INVOICE FICT-77"),
      pago("OTRO FICT USD", "014180004444555566", "014", "SPD", 99.99, "ANTICIPO FICTICIO")),
    cuenta_origen = ORIGEN_PPL, fecha = FECHA_PPL, consecutivo = 1L, moneda = "USD", clasificacion = "03"),
  # A 40-series bank outside BanBajío's catalog (prefix 143) goes "as before"
  # (40143), and the app's own "no account" line (CLABE in zeros) carries 40000.
  fuera_catalogo = list(
    pagos = rbind(
      pago("PROV FICT CIBANCO", "143180007777888899", "143", "SPI", 4321, "FACTURA FICT-143"),
      pago("SIN CUENTA FICT", "000000000000000000", "", "SPI", 10, "LINEA SIN CUENTA")),
    cuenta_origen = ORIGEN_PPL, fecha = FECHA_PPL, consecutivo = 1L, moneda = "MXN", clasificacion = NULL),
  # The individual file of a day's second batch: consecutive 002 in the header.
  individual_002 = list(
    pagos = pago("PROVEEDOR FICT UNO", "012180001234567891", "012", "SPI", 777.7, "PAGO INDIVIDUAL FICT"),
    cuenta_origen = ORIGEN_PPL, fecha = FECHA_PPL, consecutivo = 2L, moneda = "MXN", clasificacion = NULL)
)
