# =============================================================================
# tests/test_t104_actividad.R
# T104: Usuarios -> Actividad shows each confirmation's origin account,
# masked (R/audit_log_viewer_module.R), without writing anything.
#   1  .audit_con_cuentas(): a confirmation's account as shown -- the
#      snapshot its log keeps (T104), else its cuenta_id in the catalog of
#      the row's client as it is now (read once per client, only for rows
#      that need it), else "No registrada"; a payment's description says it
#      (as shown, the stored text untouched); a collection keeps its
#      description; any other row is left alone
#   2  the detail (ℹ) and the PDF: "Cuenta origen: ••••0201 · …", never the
#      cuenta_id nor the whole number; a collection's says "Cuenta:"
# Run from project root:  Rscript tests/test_t104_actividad.R
# =============================================================================
cat("=== test_t104_actividad ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/s3_memoria.R")
source("tests/helpers/server_usuarios_archivos.R")
cargar_server_usuarios()
source("tests/helpers/cuenta_origen_t104.R")

NR  <- tr("tablas.archivos_pago.cuenta_no_registrada")
TXT <- "••••0201 · NCSBB — BanBajío"
FOTO <- list(ultimos4 = "0201", alias = "NCSBB", banco = "BanBajío")
fila <- function(id, action, meta, cid = "acme", desc = "2 pago(s) confirmado(s)", module = "pagar_hoy")
  data.frame(id = id, ts = as.POSIXct("2026-11-25 10:00", tz = "UTC"), user = "luis", module = module, action = action,
             description = desc, target_id = "NCS", client_id = cid,
             metadata = as.character(jsonlite::toJSON(meta, auto_unbox = TRUE)), stringsAsFactors = FALSE)
df <- rbind(
  fila("n1", "confirmar_pago", list(empresa = "NCS", moneda = "MXN", cuenta_id = "CT-NCS-MXN", cuenta_origen = FOTO)),
  fila("v1", "confirmar_pago", list(empresa = "NCS", moneda = "MXN", cuenta_id = "CT-NCS-MXN")),
  fila("v2", "confirmar_pago", list(empresa = "NCS", moneda = "MXN", cuenta_id = "CT-NCS-MXN")),
  fila("v3", "confirmar_pago", list(empresa = "NCS", moneda = "MXN", cuenta_id = "borrada")),
  fila("v4", "confirmar_pago", list(empresa = "NCS", moneda = "MXN", cuenta_id = NA)),
  fila("o1", "confirmar_pago", list(empresa = "NCS", moneda = "MXN", cuenta_id = "CT-NCS-MXN"), cid = "otro"),
  fila("c1", "confirmar_cobro", list(empresa = "NCS", moneda = "MXN", cuenta_id = "CT-NCS-MXN"), desc = "1 cobro(s)"),
  fila("x1", "login", list(), desc = "Inicio de sesión", module = "auth"))
LECTURAS <- character()
catalogo <- function(cid) {
  LECTURAS <<- c(LECTURAS, cid)
  if (identical(cid, "otro")) stop("S3 down")
  list(ctas = CTAS_T104, bancos = BANCOS_T104)
}
out <- .audit_con_cuentas(df, catalogo)
tx <- setNames(out$cuenta_texto, out$id)

# ── 1 · The account of each row ──────────────────────────────────────────────
ok("1 · con la foto (desde T104): enmascarada, sin leer el catálogo para esa fila", identical(tx[["n1"]], TXT))
ok("1 · de antes de T104: su cuenta_id en el catálogo de hoy", identical(tx[["v1"]], TXT) && identical(tx[["v2"]], TXT))
ok("1 · cuenta borrada o sin cuenta: \"No registrada\"", identical(tx[["v3"]], NR) && identical(tx[["v4"]], NR))
ok("1 · catálogo ilegible de su cliente: \"No registrada\", nunca un error ni otra cuenta", identical(tx[["o1"]], NR))
ok("1 · el catálogo se lee una vez por cliente, solo si hace falta", identical(sort(LECTURAS), c("acme", "otro")))
ok("1 · la descripción del pago dice su cuenta (al mostrarla)",
   identical(out$description[out$id == "n1"], tr("tablas.actividad.con_cuenta", descripcion = "2 pago(s) confirmado(s)", cuenta = TXT)))
ok("1 · el cobro tiene su cuenta pero conserva su descripción; otra fila, nada",
   identical(tx[["c1"]], TXT) && out$description[out$id == "c1"] == "1 cobro(s)" &&
     is.na(tx[["x1"]]) && out$description[out$id == "x1"] == "Inicio de sesión")
ok("1 · lo guardado no cambia (metadata intacta)", identical(out$metadata, df$metadata))
LECTURAS <- character()
invisible(.audit_con_cuentas(df[df$id %in% c("n1", "x1"), ], catalogo))
ok("1 · sin filas viejas, no se lee ningún catálogo", !length(LECTURAS))

# ── 2 · Detail and PDF ───────────────────────────────────────────────────────
html <- function(x) paste(as.character(x), collapse = "")
d_n1 <- html(.audit_detail_modal_body(out[out$id == "n1", ]))
d_v1 <- html(.audit_detail_modal_body(out[out$id == "v1", ]))
d_c1 <- html(.audit_detail_modal_body(out[out$id == "c1", ]))
d_sin <- html(.audit_detail_modal_body(df[df$id == "v1", ]))   # a row the viewer did not go through
ok("2 · el detalle (ℹ): \"Cuenta origen:\" enmascarada", grepl(tr("tablas.actividad.cuenta_origen"), d_n1, fixed = TRUE) &&
     grepl(htmltools::htmlEscape(TXT), d_n1, fixed = TRUE) && grepl(htmltools::htmlEscape(TXT), d_v1, fixed = TRUE))
ok("2 · nunca el cuenta_id (\"CT-NCS-MXN\") ni más de 4 dígitos de la cuenta",
   !grepl("CT-NCS-MXN", paste(d_n1, d_v1, d_c1, d_sin), fixed = TRUE) && !grepl("73543010201|3543010201", paste(d_n1, d_v1)))
ok("2 · el cobro dice \"Cuenta:\" (la que recibe), no \"Cuenta origen:\"",
   grepl(paste0("<strong>\\s*", tr("tablas.actividad.cuenta_cobro")), d_c1, perl = TRUE) &&
     !grepl(tr("tablas.actividad.cuenta_origen"), d_c1, fixed = TRUE))
ok("2 · sin pasar por el visor, una fila vieja no adivina: \"No registrada\"",
   grepl(htmltools::htmlEscape(NR), d_sin, fixed = TRUE))
p_n1 <- html(.audit_pdf_entry_card(out[out$id == "v1", ]))
ok("2 · la tarjeta del PDF: la misma cuenta enmascarada", grepl(htmltools::htmlEscape(TXT), p_n1, fixed = TRUE) &&
     !grepl("CT-NCS-MXN", p_n1, fixed = TRUE))
doc <- tryCatch(generate_audit_log_html_doc(out, list(generated_by = "luis")), error = function(e) e)
doc_txt <- if (is.character(doc) && file.exists(doc[1])) paste(readLines(doc[1], encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""
ok("2 · el PDF entero: la cuenta en la tabla y en el detalle, ningún cuenta_id",
   nzchar(doc_txt) && grepl("0201", doc_txt, fixed = TRUE) && !grepl("CT-NCS-MXN", doc_txt, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
