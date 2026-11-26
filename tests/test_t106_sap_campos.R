# =============================================================================
# tests/test_t106_sap_campos.R
# T106 2c: scripts/diagnose_t106_sap_campos_readonly.R tells which SAP field
# holds the CFDI's UUID -- printing FIELD NAMES ONLY, never a value, and
# writing nothing. Against the fake Service Layer (tests/helpers/sap_falso.R)
# and the in-memory S3; invented data only.
#   1  the classifier: fiscal names, "con valor"/"vacío", a value shaped like
#      a UUID found -- and no value ever in what it prints
#   2  end to end: one GET per company and endpoint ($top=1); names printed;
#      none of the record's values on the console; nothing written to S3
#   3  no VPN: exit 2 with a clear message
#
# Run from project root:  Rscript tests/test_t106_sap_campos.R
# =============================================================================
cat("=== test_t106_sap_campos ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr, detalle = NULL) {
  r <- tryCatch(isTRUE(expr), error = function(e) { message("  error: ", conditionMessage(e)); FALSE })
  if (r) { .pass <<- .pass + 1L; cat(" PASS:", label, "\n") }
  else   { .fail <<- .fail + 1L; cat(" FAIL:", label, "\n"); if (!is.null(detalle)) cat("       ", detalle, "\n") }
}
source("tests/helpers/s3_memoria.R")
source("tests/helpers/sap_falso.R")
sys.source("scripts/diagnose_t106_sap_campos_readonly.R", envir = globalenv())

cat("== 1. El clasificador ==\n")
UUID <- "C0FFEE00-1234-4abc-9def-00000000beef"
fila <- list(DocNum = 9001, NumAtCard = "F-SECRETO-77", CardName = "PROVEEDOR SECRETO SA",
             U_UUID = UUID, U_FolioFiscal = "", EDocNum = NULL, U_Otro = "valor secreto", DocTotal = 123.45)
res <- diag_campos_resumen(fila)
ok("1.1 nombres fiscales: U_UUID, U_FolioFiscal, EDocNum, U_Otro", setequal(res$campo[res$fiscal], c("U_UUID", "U_FolioFiscal", "EDocNum", "U_Otro")))
ok("1.2 con valor / vacío", res$con_valor[res$campo == "U_UUID"] && !res$con_valor[res$campo == "U_FolioFiscal"] && !res$con_valor[res$campo == "EDocNum"])
ok("1.3 detecta el campo cuyo VALOR tiene forma de UUID", identical(res$campo[res$forma_uuid], "U_UUID"))
lin <- paste(diag_lineas("NG", "PurchaseInvoices", res), collapse = "\n")
ok("1.4 lo impreso nombra U_UUID y no trae NINGÚN valor (ni el UUID)",
   grepl("U_UUID", lin, fixed = TRUE) && !any(vapply(c(UUID, "F-SECRETO-77", "PROVEEDOR SECRETO", "valor secreto", "123.45", "9001"),
                                                   function(v) grepl(v, lin, fixed = TRUE), logical(1))))

cat("== 2. De punta a punta ==\n")
CID <- "demo"
sap <- sap_falso_iniciar(list(NG = list(AR = 3, AP = 2, modo = "ok")))
on.exit(sap$parar(), add = TRUE)
do.call(Sys.setenv, stats::setNames(list(sap$url_de("NG"), "BD_FICTICIA", "usuario.ficticio", "CLAVE-FICTICIA-9"),
                                    paste0("SAP_NG", c("_URL", "_COMPANY", "_USER", "_PASSWORD"))))
Sys.setenv(HOPDESK_PRODUCTION = "1", AWS_ACCESS_KEY_ID = "ficticia", AWS_SECRET_ACCESS_KEY = "ficticia", AWS_DEFAULT_REGION = "us-east-1")
s3_memoria_reset()
.mem[[paste0(CID, "/empresas.rds")]] <- tibble::tibble(id = "e1", account_code = "E0001", initials = "NG",
  razon_social = "EMPRESA FICTICIA SA", nombre_corto = "Ficticia Norte", activa = TRUE, deleted = FALSE)
n0 <- .mem_escrituras; sap$log_borrar()
salida <- utils::capture.output(codigo <- suppressWarnings(diag_main(c("--cliente", CID, "--produccion"))))
s3_memoria_reinstalar()
lg <- sap$log(); lg <- lg[lg$endpoint %in% c("Invoices", "PurchaseInvoices"), ]   # the fake also logs the final Logout
ok("2.1 termina con 0 y nombra los campos de cada libro", identical(as.integer(codigo), 0L) &&
     sum(grepl("campos$", salida)) == 2L && any(grepl("NumAtCard", salida, fixed = TRUE) | grepl("nombre fiscal", salida, fixed = TRUE)),
   paste(salida, collapse = " | "))
ok("2.2 una sola consulta por libro, de un registro", nrow(lg) == 2L && all(lg$filas <= 1L) &&
     setequal(lg$endpoint, c("Invoices", "PurchaseInvoices")))
ok("2.3 ningún valor del registro sale en la consola (ni la contraseña ni la URL)",
   !any(grepl("CLIENTE FICTICIO|F-NG-|XAXX010101000|CLAVE-FICTICIA|127.0.0.1", salida)))
ok("2.4 no escribe nada en S3", .mem_escrituras == n0)

cat("== 3. Sin VPN ==\n")
Sys.setenv(SAP_NG_URL = sprintf("http://127.0.0.1:%d/NG/b1s/v1", httpuv::randomPort()))
rm(list = ls(.sap_sessions), envir = .sap_sessions)
salida <- utils::capture.output(codigo <- suppressWarnings(diag_main(c("--cliente", CID, "--produccion"))))
ok("3.1 código 2 y el mensaje dice VPN", identical(as.integer(codigo), 2L) && any(grepl("VPN", salida, fixed = TRUE)))
Sys.unsetenv("HOPDESK_PRODUCTION")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
s3_memoria_restaurar()
if (.fail > 0L) quit(status = 1L)
