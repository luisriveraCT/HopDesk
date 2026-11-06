# =============================================================================
# tests/helpers/abono_datos_prueba.R
# Test data for Abono parcial (R/staging_browse_module.R, T25), relative to
# `hoy` so it always falls in the modal's default window (hoy -15 .. hoy +15).
# abono_datos_prueba(hoy) -> list(sap = list(AP, AR), abonos, pagar_hoy, esperado)
#   AP, NCS · MXN · Proveedor Uno: F-101 (1,000), F-102 (2,500.50, Factura
#       A-77), F-103 (400 - 150 confirmed = 250)       -> a group of 3
#   AP, NCS · USD · Proveedor Dos: F-201 (800, code " P002 ")  -> alone
#   AP, NTS · MXN · Proveedor Uno: F-501 (600)  -> alone (other company)
#   AP, NTS · MXN · Proveedor Tres: F-301, fully paid by a confirmed abono -> out
#   AP, NTS · MXN · Proveedor Cuatro: F-403 due hoy+15 (in), F-401 hoy+20 and
#       F-402 hoy-16 (out of the window)
#   AR, NCS · MXN · Cliente Uno: F-901 (5,000, code C001)
# A voided abono (F-101), an AR abono on F-101 (AR's, not AP's) and an abono
# still pending in the Agenda (F-102) change no balance.
# =============================================================================
abono_datos_prueba <- function(hoy = Sys.Date()) {
  ap <- tibble::tibble(
    Empresa   = c("NCS", "NCS", "NCS", "NCS", "NTS", "NTS", "NTS", "NTS", "NTS"),
    Moneda    = c("MXN", "MXN", "MXN", "USD", "MXN", "MXN", "MXN", "MXN", "MXN"),
    Documento = c("F-101", "F-102", "F-103", "F-201", "F-501", "F-301", "F-403", "F-401", "F-402"),
    Factura   = c("", "A-77", "", "", "", "", "", "", ""),
    Parte     = c("Proveedor Uno", "Proveedor Uno", "Proveedor Uno", "Proveedor Dos", "Proveedor Uno",
                  "Proveedor Tres", "Proveedor Cuatro", "Proveedor Cuatro", "Proveedor Cuatro"),
    `Código de proveedor` = c("P001", "P001", "P001", " P002 ", "P001", "P003", "P004", "P004", "P004"),
    `Fecha de vencimiento` = hoy + c(-3L, 2L, 5L, 1L, 7L, 0L, 15L, 20L, -16L),
    `Saldo vencido` = c(1000, 2500.5, 400, 800, 600, 300, 50, 70, 90))
  ar <- tibble::tibble(
    Empresa = "NCS", Moneda = "MXN", Documento = "F-901", Factura = "", Parte = "Cliente Uno",
    `Código de cliente` = "C001", `Fecha de vencimiento` = hoy, `Saldo vencido` = 5000)
  abonos <- tibble::tibble(
    id = c("ab1", "ab2", "ab3", "ab4"), ledger = c("AP", "AP", "AP", "AR"),
    Empresa = c("NCS", "NCS", "NTS", "NCS"), Moneda = "MXN",
    Documento = c("F-103", "F-101", "F-301", "F-101"), Parte = "x",
    importe = c(150, 100, 300, 999), fecha_abono = hoy, notas = "", created_by = "dev",
    created_at = Sys.time(), status = c("active", "voided", "active", "active"))
  pagar_hoy <- tibble::tibble(
    id = "ph1", ledger = "AP", Empresa = "NCS", Moneda = "MXN", Documento = "F-102", Parte = "Proveedor Uno",
    Codigo = "P001", tipo_item = "abono", Importe = 100, FechaVenc = hoy + 2L, staged_by = "otro",
    staged_at = Sys.time(), status = "pending", source = "manual")
  # What the AP table shows by default, in its order (due date): Documento,
  # reference, balance, its group (Parte · Empresa · Moneda) or NA when alone.
  esperado <- tibble::tibble(
    Documento = c("F-101", "F-201", "F-102", "F-103", "F-501", "F-403"),
    Referencia = c("F-101", "F-201", "A-77", "F-103", "F-501", "F-403"),
    Saldo = c(1000, 800, 2500.5, 250, 600, 50),
    Grupo = c("Proveedor Uno|NCS|MXN", NA, "Proveedor Uno|NCS|MXN", "Proveedor Uno|NCS|MXN", NA, NA))
  list(sap = list(AP = ap, AR = ar), abonos = abonos, pagar_hoy = pagar_hoy, esperado = esperado)
}
