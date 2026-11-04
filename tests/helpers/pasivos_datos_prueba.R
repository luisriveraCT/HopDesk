# =============================================================================
# tests/helpers/pasivos_datos_prueba.R
# Pasivos test data, ONE file for every Pasivos test (T17b, the table; T18,
# the list and the wizard): add what you need and say so in the bitácora.
# Never S3: plain tibbles in the app's schemas (R/pasivos_schemas.R).
#
#   d <- pasivos_datos_prueba(hoy = Sys.Date())
#   d$liabilities, d$provisions, d$manual_inv (AP), d$bancos_confirmados,
#   d$visibles (the companies the user sees)
#
# Dates are relative to `hoy` (pasivos_table_build_long() reads Sys.Date()).
# What each piece is for (T17b paso 0, reportes/T17b_paso0.md):
#   L1 Renta oficina      regular MXN NTS: overdue / today / +1 / +5 / +20 (amount by
#                         hand) / +30 (date by hand); same day as a converted item (+20);
#                         closed, deleted, unknown and dateless provisions; -8 and +54
#                         (outside the window)
#   L2 Crédito Banorte    financiero USD BOD: two initial fees (+2) and a monthly (+10);
#                         items pending (+3), overdue (-1), confirmed (-2) and one whose
#                         confirmation was deleted (-4)
#   L3 Tarjeta Amex       tarjeta EUR NTS: a provision and an item in one ISO week
#   L4 Licencias <b>…     regular CAD NRS: HTML, quotes and accents; its id has a '
#   L5 Luz bodega         regular MXN, company OCU (not visible)
#   L6 Arrendamiento      regular MXN NTS, archived (estado "deleted") with an overdue one
#   orphan                manual provision without liability, NRS, +4
# =============================================================================

pasivos_datos_prueba <- function(hoy = Sys.Date()) {
  hoy <- as.Date(hoy)
  liab <- function(id, nombre, categoria, subcategoria, empresa, parte, moneda, estado = "active")
    tibble::add_row(.schema_pasivos_liability(), id = id, nombre = nombre, categoria = categoria,
                    subcategoria = subcategoria, empresa = empresa, parte = parte, moneda_pago = moneda,
                    estado = estado)
  liabilities <- dplyr::bind_rows(
    liab("lib1", "Renta oficina", "regular", "renta", "NTS", "Inmobiliaria Sol", "MXN"),
    liab("lib2", "Crédito Banorte", "financiero", "credito", "BOD", "Banorte", "USD"),
    liab("lib3", "Tarjeta Amex", "tarjeta", "tarjeta", "NTS", "American Express", "EUR"),
    liab("lib'4", "Licencias <b>Ñandú</b> & 'Co'", "regular", "software", "NRS", "Proveedor \"X\" <i>", "CAD"),
    liab("lib5", "Luz bodega", "regular", "servicios", "OCU", "CFE", "MXN"),
    liab("lib6", "Arrendamiento archivado", "regular", "renta", "NTS", "Arrendadora", "MXN", estado = "deleted"))

  prov <- function(id, lid, dias, monto, estado = "provisional", origin = "rule", occ = NA_integer_,
                   empresa = NULL, parte = NULL, moneda = NULL, referencia = NA_character_,
                   override_monto = NA_real_, override_fecha = NA, fecha = hoy + dias) {
    l <- liabilities[match(lid, liabilities$id), ]
    tibble::add_row(.schema_pasivos_provision(), id = id, liability_id = lid, origin = origin,
                    occurrence_index = as.integer(occ), estado = estado,
                    fecha_calculada = as.Date(fecha), fecha_efectiva = as.Date(fecha),
                    empresa = empresa %||% l$empresa, parte = parte %||% l$parte,
                    moneda_pago = moneda %||% l$moneda_pago, amount_pago = monto,
                    amount_pago_override = override_monto, fecha_efectiva_override = as.Date(override_fecha),
                    referencia = referencia, reverted_count = 0L)
  }
  # The Monday of next week (the week of hoy + 7): L3's two in one ISO week.
  lunes <- hoy + 7L - ((as.POSIXlt(hoy + 7L)$wday + 6L) %% 7L)
  provisions <- dplyr::bind_rows(
    prov("p1a", "lib1", -3, 1000),
    prov("p1b", "lib1", 0, 1100),
    prov("p1c", "lib1", 1, 1200),
    prov("p1d", "lib1", 5, 1300),
    prov("p1e", "lib1", 20, 1400, override_monto = 1450),
    prov("p1f", "lib1", 30, 1500, override_fecha = hoy + 30),
    prov("p1h", "lib1", 20, 500, estado = "converted"),
    prov("p1x", "lib1", 6, 10, estado = "closed"),
    prov("p1y", "lib1", 7, 20, estado = "deleted"),
    prov("p1z", "lib1", 8, 30, estado = "en_revision"),
    prov("p1n", "lib1", 9, 40, fecha = NA),
    prov("p1i", "lib1", -8, 900),
    prov("p1j", "lib1", 54, 950),
    prov("p2f1", "lib2", 2, 250, origin = "initial_fee", occ = -1L, referencia = "Comisión por apertura"),
    prov("p2f2", "lib2", 2, 120, origin = "initial_fee", occ = -2L, referencia = "Comisión por disposición"),
    prov("p2m", "lib2", 10, 5000),
    prov("p2p", "lib2", 3, 700, estado = "converted"),
    prov("p2v", "lib2", -1, 710, estado = "converted"),
    prov("p2c", "lib2", -2, 720, estado = "item_confirmed"),
    prov("p2e", "lib2", -4, 730, estado = "converted"),
    prov("p3a", "lib3", 0, 300, fecha = lunes),
    prov("p3b", "lib3", 0, 50, estado = "converted", fecha = lunes + 1L),
    prov("p4", "lib'4", 3, 60),
    prov("p5", "lib5", 2, 777),
    prov("p6", "lib6", -5, 666),
    prov("p0", NA_character_, 4, 80, origin = "manual", empresa = "NRS", parte = "Papelería Centro", moneda = "MXN"))

  item <- function(id, provision_id, fecha, importe, empresa)
    tibble::tibble(id = id, provision_id = provision_id, FechaVenc = as.Date(fecha), Importe = importe,
                   ledger = "AP", empresa = empresa, Parte = "", Moneda = "")
  manual_inv <- dplyr::bind_rows(
    item("mi1h", "p1h", hoy + 20, 500, "NTS"),
    item("mi2p", "p2p", hoy + 3, 700, "BOD"),
    item("mi2v", "p2v", hoy - 1, 710, "BOD"),
    item("mi2c", "p2c", hoy - 2, 720, "BOD"),
    item("mi2e", "p2e", hoy - 4, 730, "BOD"),
    item("mi3b", "p3b", lunes + 1L, 50, "NTS"))

  bancos_confirmados <- tibble::tibble(id = c("bc2c", "bc2e"), provision_id = c("p2c", "p2e"),
                                       eliminado = c(FALSE, TRUE))

  list(liabilities = liabilities, provisions = provisions, manual_inv = manual_inv,
       bancos_confirmados = bancos_confirmados, visibles = c("NTS", "BOD", "NRS"), hoy = hoy, lunes = lunes)
}
