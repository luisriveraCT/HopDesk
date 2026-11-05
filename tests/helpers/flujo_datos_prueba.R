# =============================================================================
# tests/helpers/flujo_datos_prueba.R
# Flujo de caja test data (T17c): the rows build_export_combined_df() would
# give (AR + AP), relative to a Monday, and the tags. Never S3.
#
#   d <- flujo_datos_prueba(lunes)
#   d$df (rows), d$tags (tags_db), d$empresas (the selected ones)
#
# What each row is for:
#   MXN AR  CLIENTE ALFA  L 4,495,962 (millions) + X 40,000; tagged urgent
#           CLIENTE BETA  M 802,000 (thousands); tagged important
#           CLIENTE GAMA  J 950 (under a thousand) -- a provision (source)
#   MXN AP  PROVEEDOR UNO L 5,000,000 (net of Monday negative), V 120.5
#           PROVEEDOR DOS M 802,000 (net of Tuesday exactly 0)
#   USD AR  CLIENT USA    M 10,000;  USD AP  VENDOR USA  X 2,500
#   out of the week (lunes - 1, lunes + 7) and another company: never shown
# =============================================================================
flujo_datos_prueba <- function(lunes) {
  lunes <- as.Date(lunes)
  fila <- function(tipo, parte, dia, importe, moneda = "MXN", empresa = "NETWORKS TRANS", fuente = "sap")
    tibble::tibble(Tipo = tipo, Parte = parte, Empresa = empresa, Moneda = moneda, FechaEff = lunes + dia,
                   `Saldo vencido` = importe, source = fuente, Documento = paste0(tipo, "-", parte, "-", dia))
  df <- dplyr::bind_rows(
    fila("AR", "CLIENTE ALFA", 0, 4495962), fila("AR", "CLIENTE ALFA", 2, 40000),
    fila("AR", "CLIENTE BETA", 1, 802000, empresa = "NETWORKS CARGO"),
    fila("AR", "CLIENTE GAMA", 3, 950, fuente = "provision"),
    fila("AP", "PROVEEDOR UNO", 0, 5000000), fila("AP", "PROVEEDOR UNO", 4, 120.5),
    fila("AP", "PROVEEDOR DOS", 1, 802000),
    fila("AR", "CLIENT USA", 1, 10000, moneda = "USD"),
    fila("AP", "VENDOR USA", 2, 2500, moneda = "USD"),
    fila("AR", "FUERA ANTES", -1, 111), fila("AR", "FUERA DESPUES", 7, 222),
    fila("AR", "OTRA EMPRESA", 0, 333, empresa = "OTRA SA"))
  tags <- tibble::tibble(ledger = c("AR", "AR"), Empresa = c("NETWORKS TRANS", "NETWORKS CARGO"), Moneda = "MXN",
                         Documento = c("AR-CLIENTE ALFA-0", "AR-CLIENTE BETA-1"), tag = c("urgent", "important"))
  list(df = df, tags = tags, empresas = c("NETWORKS TRANS", "NETWORKS CARGO"), lunes = lunes)
}
