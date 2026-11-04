# =============================================================================
# tests/test_ajustes_ic_registro.R
# The saved IC registry in Ajustes › Intercompany (ic_registro, T16) on the
# table engine:
#   1. .ic_registro_filas(): one row per company of the map with a code, its
#      codes with their prefix; an empty or NULL registry gives no rows but
#      keeps the columns (a columnless frame closed a client's session, b472f02);
#   2. the real settings_observers() start with the registry and the company
#      map NULL (as before S3 loads) and open the IC panel without the
#      session failing or the engine reporting "the datos failed";
#   3. the table follows the saved registry: a save elsewhere (another
#      session through the sync bus) shows up without reopening the panel,
#      and the editor's re-draw (ic_trigger) leaves the table's container alone.
#
# Run from project root:  Rscript tests/test_ajustes_ic_registro.R
# =============================================================================
cat("=== test_ajustes_ic_registro ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a)) b else a

for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/settings/settings_cuentas.R", "R/settings/settings_bp.R", "R/settings/settings_hub.R"))
  suppressMessages(source(f))
log_action <- function(...) invisible(NULL)
showNotification <- function(...) invisible("id")
showModal <- function(...) invisible(NULL)
insertUI <- function(...) invisible(NULL)
load_sap_snapshot <- function(...) list(data = NULL)
proveedoresServer <- function(...) invisible(NULL)
show_settings_modal <- function(...) invisible(NULL)
GUARDADOS <- 0L
save_interco_v2 <- function(...) { GUARDADOS <<- GUARDADOS + 1L; invisible(NULL) }
bump_sync_version <- function(...) invisible(NULL)

CMAP <- c(NTS = "Networks Trucking", NCS = "Networks Crossdocking", NRS = "Networks Realtors")
REG  <- list(ar_prefix = "C", ap_prefix = "P", rfcs = character(),
             companies = list(NTS = list(ar = c("1027", "2001"), ap = "1426"),
                              NCS = list(ar = character(), ap = character()),
                              NRS = list(ar = character(), ap = "3003"),
                              XXX = list(ar = "9", ap = character())))

# ── 1. The rows ──────────────────────────────────────────────────────────────
f <- .ic_registro_filas(REG, CMAP)
ok("una fila por empresa del mapa con códigos (NTS y NRS), en el orden del mapa",
   identical(f$id, c("NTS", "NRS")) && identical(f$Empresa, c("NTS", "NRS")))
ok("los códigos llevan su prefijo, separados por coma; sin códigos, —",
   identical(f$cxc, c("C1027, C2001", "—")) && identical(f$cxp, c("P1426", "P3003")))
ok("un prefijo propio del registro se respeta",
   identical(.ic_registro_filas(modifyList(REG, list(ar_prefix = "CL")), CMAP)$cxc[1], "CL1027, CL2001"))
COLS <- c("id", "Empresa", "cxc", "cxp")
for (caso in list(list("registro NULL", NULL, CMAP), list("registro sin empresas", list(companies = list()), CMAP),
                  list("ninguna empresa con códigos", { r0 <- REG; r0$companies <- list(NCS = list()); r0 }, CMAP),
                  list("mapa de empresas NULL", REG, NULL)))
  ok(sprintf("%s: 0 filas, con sus columnas", caso[[1]]),
     { v <- .ic_registro_filas(caso[[2]], caso[[3]]); is.data.frame(v) && nrow(v) == 0L && identical(names(v), COLS) })

# ── 2 and 3. On the real observers ───────────────────────────────────────────
correr <- function(reg0, cmap0, clics) {
  avisos <- character(0)
  error <- tryCatch(withCallingHandlers({
    shared <- list(usuario = reactiveVal(list(permisos = c(can_manage_interco = TRUE))),
                   current_user = function() "dev", effective_client_id = function() "acme",
                   home_client_id = function() "acme", company_map = reactiveVal(cmap0),
                   interco_v2 = reactiveVal(reg0), proveedores_db = reactiveVal(NULL), sap_data = function() list())
    testServer(function(input, output, session) settings_observers(input, output, session, shared),
               { session$flushReact(); clics(session, output, shared) })
    NULL
  }, message = function(m) { avisos <<- c(avisos, conditionMessage(m)); invokeRestart("muffleMessage") }),
  error = function(e) conditionMessage(e))
  list(error = error, fallas = grep("the (datos|aviso) failed", avisos, value = TRUE))
}

r <- correr(NULL, NULL, function(session, output, shared) {
  session$flushReact()
  session$setInputs(stg_btn_interco = 1)
  invisible(output$settings_panel); invisible(output$ic_registro_titulo)
})
if (!is.null(r$error)) message("  ", r$error)
ok("arranque con el registro y el mapa en NULL: la sesión sigue y el motor no reporta fallas",
   is.null(r$error) && !length(r$fallas))

panel1 <- panel2 <- titulo <- NULL
r <- correr(list(ar_prefix = "C", ap_prefix = "P", companies = list()), CMAP, function(session, output, shared) {
  session$setInputs(stg_btn_interco = 1)
  panel1 <<- output$settings_panel$html
  ok("sin códigos guardados: sin título de totales (el vacío lo dice la tabla)", is.null(output$ic_registro_titulo))
  shared$interco_v2(REG)                     # another session saved: the sync bus sets it
  session$flushReact()
  titulo <<- as.character(output$ic_registro_titulo$html)
  session$setInputs(ic_ar_NTS_1 = "1027")
  session$setInputs(save_interco_v2 = 1)     # Guardar bumps ic_trigger: re-draws only the editor
  panel2 <<- output$settings_panel$html
})
# The totals count the whole registry (XXX too, not in the map), as the old card did.
ok("lo guardado en otra sesión aparece sin reabrir: el título cuenta 3 CxC y 2 CxP",
   is.null(r$error) && grepl("3 código(s) CxC · 2 código(s) CxP", titulo, fixed = TRUE))
ok("Guardar repinta el editor, pero no el panel con la tabla (la tabla conserva búsqueda y scroll)",
   GUARDADOS == 1L && !is.null(panel1) && identical(panel1, panel2) && grepl("ic_registro_t", panel1, fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
