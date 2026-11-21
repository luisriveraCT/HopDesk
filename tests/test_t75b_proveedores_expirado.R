# =============================================================================
# tests/test_t75b_proveedores_expirado.R
# T75b (CM27): Proveedores' "expirado" badge compares against the USER's day
# (hoy_usuario()), what each person sees. With Sys.Date() -- the server's day,
# UTC in prod -- a validity ending today showed "expirado" from 18:00 on in
# Mexico. The real proveedoresServer in testServer
# (tests/helpers/proveedores_testserver.R), the process in UTC like prod,
# the clock at 19:30 of the 20th in Mexico (01:30 of the 21st, UTC).
# Run from project root:  Rscript tests/test_t75b_proveedores_expirado.R
# Contraprueba: on 7cc6ce3 (before this change) the first check fails.
# =============================================================================
Sys.setenv(TZ = "UTC")
cat("=== test_t75b_proveedores_expirado ===\n\n")
source("tests/helpers/proveedores_testserver.R")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
AHORA <- as.POSIXct("2026-11-21 01:30:00", tz = "UTC")
assign("Sys.time", function() AHORA, envir = globalenv())
assign("Sys.Date", function() as.Date(format(AHORA, "%Y-%m-%d", tz = "UTC")), envir = globalenv())   # the server's: the 21st

activos <- rbind(prov_fila("p-hoy",  "VENCE HOY",  "012180001234567890", activo_hasta = "2026-11-20"),
                 prov_fila("p-ayer", "VENCIO AYER", "012180001234567891", activo_hasta = "2026-11-19"),
                 prov_fila("p-ind",  "SIN FECHA",  "012180001234567892"))
shared <- list(
  proveedores_db = reactiveVal(activos), proveedores_inactivos_db = reactiveVal(activos[0, ]),
  company_map = reactive(c(NTS = "Networks Trucking")), effective_client_id = function() "prueba",
  home_client_id = function() "prueba", current_user = function() "prueba", usuario = function() "prueba",
  parte_alias_map_db = reactiveVal(NULL))
G <- new.env()
suppressWarnings(suppressMessages(shiny::testServer(proveedoresServer, args = list(shared = shared), {
  session$rootScope()$setInputs(client_tz = "America/Mexico_City")
  session$flushReact()
  G$d <- catalogo_datos()
})))
vig <- function(id) G$d$vigencia[G$d$id == id]
ok("a validity ending today (the 20th), at 19:30 in Mexico: no 'expirado' (the server's day is already the 21st)",
   identical(vig("p-hoy"), ""))
ok("one that ended yesterday: 'expirado'", identical(vig("p-ayer"), "expirado"))
ok("one with no end date: no badge", identical(vig("p-ind"), ""))

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
