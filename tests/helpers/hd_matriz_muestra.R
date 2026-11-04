# A table with every column type and every control column, none of T17a's
# options: what tests/test_hd_matriz_r.R compares byte for byte against
# tests/fixtures/hd_matriz_antes.json (built with the code before T17a).
hd_matriz_muestra <- function() {
  datos <- data.frame(
    id = c("a", "b", "c", "d"), Empresa = c("Networks Crossdocking Services", "NTS", "BOD", ""),
    Parte = c("ACEROS", "PAPEL <b>", NA, "Ñandú"), Importe = c(1234.5, -0.001, 0, NA),
    Saldo = c(4495962, 802000, 999.6, -1500), Cantidad = c(3, 1200, NA, 0),
    Fecha = as.Date(c("2026-11-04", NA, "2026-01-31", "2025-12-01")),
    Hora = c("2026-11-02 08:40:54", NA, "2026-11-02 20:00", "x"),
    Estado = c("pagada", "pendiente", "otra", NA), Etiqueta = c("urgent", "", "both", "important"),
    Moneda = c("MXN", "USD", "MXN", "MXN"), .tipo = c("cobro", "pago", "x", NA),
    .bloqueada = c(FALSE, TRUE, NA, FALSE), .provision = c(TRUE, FALSE, FALSE, NA),
    .fijar = c(FALSE, TRUE, FALSE, FALSE), .agregado = c(TRUE, FALSE, NA, FALSE),
    .adjuntos = c(2, NA, -1, 0), .acciones = c("editar", "", NA, "editar,eliminar"),
    stringsAsFactors = FALSE)
  columnas <- list(
    col_empresa("Empresa", iniciales = c(NCS = "Networks Crossdocking Services")),
    col_texto("Parte", "tablas.columna.parte", ancho = 120),
    col_dinero("Importe"), col_dinero("Saldo", sumar = FALSE, opcional = TRUE, visible = FALSE),
    col_numero("Cantidad", decimales = 1), col_fecha("Fecha"), col_fecha_hora("Hora"),
    col_insignia("Estado", mapa = list(pagada = list(texto = "Pagada", tono = "exito"), pendiente = list(texto = "Pendiente"))),
    col_etiqueta("Etiqueta"))
  list(datos = datos, columnas = columnas)
}
