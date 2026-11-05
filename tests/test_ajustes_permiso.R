# =============================================================================
# tests/test_ajustes_permiso.R
# The 7 saves of Ajustes check their permission on the server (T16a): the
# gear opens for every user, a client included. can_manage_bancos guards the
# bank catalog and the company accounts (the PPL file's source account);
# can_manage_interco guards the IC registry (what is paid between companies):
# a client's Admin edits it, Finanzas does not (Luis, 2026-11-04).
#
# On the real observers in testServer over the CM09's in-memory S3
# (tests/helpers/s3_memoria.R):
#   1. without the permission each save is refused and nothing reaches S3;
#   2. with it, the same click writes;
#   3. without it, the buttons that write are not drawn (reading still is);
#   4. static: each of the 7 handlers starts with puede_o_avisa().
#
# Run from project root:  Rscript tests/test_ajustes_permiso.R
# =============================================================================
cat("=== test_ajustes_permiso ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/settings/settings_helpers.R", c(".schema_ctas_bancos", ".schema_ctas_cuentas", "load_ctas_bancos",
                                          "load_ctas_cuentas", "modificar_ctas_bancos", "modificar_ctas_cuentas",
                                          ".BANCOS_MX_CHOICES", "borrar_banco_si_libre",
                                          ".banco_en_uso_filas", ".banco_en_uso_texto"))
cargar("R/adjuntos_s3.R", c(".NUMERO_PURO", ".CELDA_PELIGRO", "neutralizar_celdas", "tabla_a_texto_copiable"))
cargar("R/data_pipeline.R", "scan_ic_candidates")
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/settings/settings_cuentas.R", "R/settings/settings_bp.R", "R/settings/settings_hub.R"))
  suppressMessages(source(f))

# Recorders and stand-ins (after the sources, so they win). puede() and
# puede_o_avisa() are the real ones.
MODALES <- list()
log_action <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
showNotification <- function(...) invisible("id")
showModal <- function(ui, ...) { MODALES[[length(MODALES) + 1L]] <<- ui; invisible(NULL) }
removeModal <- function(...) invisible(NULL)
insertUI <- function(...) invisible(NULL)
show_settings_modal <- function(...) invisible(NULL)
proveedoresServer <- function(...) invisible(NULL)
proveedoresUI <- function(id) div()

CID  <- "acme"
CMAP <- c(NTS = "Networks Trucking")
# Real tiers: Admin has both keys; without can_manage_bancos, Analisis;
# without can_manage_interco, Finanzas (who does manage the banks).
nivel <- function(t) list(user = t, tier = t, permisos = permisos_resueltos(t))
CON   <- nivel("admin")
SIN   <- list(can_manage_bancos = nivel("analysis"), can_manage_interco = nivel("finance"))
stopifnot(puede(CON, "can_manage_bancos"), puede(CON, "can_manage_interco"),
          !puede(SIN$can_manage_bancos, "can_manage_bancos"), !puede(SIN$can_manage_interco, "can_manage_interco"),
          puede(SIN$can_manage_interco, "can_manage_bancos"))

# Two banks: .s3_write refuses to empty the catalog (deleting the last bank
# fails today -- a CM12 matter, not a permission one).
BANCOS0 <- tibble::tibble(id = c("b1", "b2"), nombre = c("BanBajío", "Santander"), clave = c("030", "014"))
CTAS0   <- tibble::tibble(id = "c1", banco_id = "b1", Empresa = "NTS", razon_social = "NT SA", rfc = "NTS010101AA1",
                          Moneda = "MXN", alias = "NT-MXN", cuenta = "0100", clabe_interbancaria = "030000000000000001",
                          is_ppl_default = TRUE, saldo_inicial = 0, activa = TRUE)
REG0    <- list(ar_prefix = "C", ap_prefix = "P", rfcs = character(),
                companies = list(NTS = list(ar = "1027", ap = character())))
SNAP_AR <- data.frame(Empresa = "Networks Trucking", `Código de cliente` = c("C1027", "C2001"),
                      Parte = c("NETWORKS CROSSDOCKING", "NETWORKS REALTORS"), RFC = c("NCS010101AA1", "NRS010101AA1"),
                      check.names = FALSE, stringsAsFactors = FALSE)
load_sap_snapshot <- function(ledger, client_id = NULL)
  list(data = if (ledger == "AR") SNAP_AR else NULL)

sembrar <- function() {
  s3_memoria_reset()
  assign(paste0(CID, "/", S3_KEYS$ctas_bancos),  BANCOS0, envir = .mem)
  assign(paste0(CID, "/", S3_KEYS$ctas_cuentas), CTAS0,   envir = .mem)
  assign(paste0(CID, "/", S3_KEYS$interco_v2),   REG0,    envir = .mem)
}
en_s3 <- function(k) get0(paste0(CID, "/", S3_KEYS[[k]]), envir = .mem)

nuevo_shared <- function(u) list(
  usuario = reactiveVal(u), current_user = function() u$user,
  effective_client_id = function() CID, home_client_id = function() CID,
  company_map = reactive(CMAP), ctas_cuentas = reactiveVal(CTAS0),
  interco_v2 = reactiveVal(REG0), proveedores_db = reactiveVal(NULL), sap_data = function() list())

# Runs `clics` on a fresh session of `servidor` for user `u`; returns the
# writes that reached S3 and the stored tables.
correr <- function(u, servidor, clics) {
  sembrar(); MODALES <<- list()
  shared <- nuevo_shared(u)
  antes <- .mem_escrituras
  # The first flush is the start-up one, which ignoreInit observers skip:
  # without it the first click of each case would be lost.
  testServer(function(input, output, session) servidor(input, output, session, shared),
             { session$flushReact(); clics(session, output) })
  list(escrituras = .mem_escrituras - antes, bancos = en_s3("ctas_bancos"),
       cuentas = en_s3("ctas_cuentas"), registro = en_s3("interco_v2"))
}

# ── The 7 saves ──────────────────────────────────────────────────────────────
GUARDADOS <- list(
  banco_save = list(servidor = settings_cuentas_observer, permiso = "can_manage_bancos",
    clics = function(session, output) {
      session$setInputs(banco_nombre = "Banorte", banco_clave = "072")
      session$setInputs(banco_save = 1)
    },
    escribio = function(r) nrow(r$bancos) == 3L && "Banorte" %in% r$bancos$nombre),
  banco_delete = list(servidor = settings_cuentas_observer, permiso = "can_manage_bancos",
    clics = function(session, output) {
      # b2: no account uses it (CTAS0's c1 is on b1, which CM17 refuses).
      session$setInputs(`bancos_cat_t-accion` = list(accion = "editar", ids = "b2", nonce = 1))   # ✎ on b2
      session$setInputs(banco_delete = 1)
    },
    escribio = function(r) identical(r$bancos$id, "b1")),
  cuenta_save = list(servidor = settings_cuentas_observer, permiso = "can_manage_bancos",
    clics = function(session, output) {
      session$setInputs(cta_empresa = "NTS", cta_cuenta = "0200", cta_moneda = "USD", cta_banco_id = "b1")
      session$setInputs(cuenta_save = 1)
    },
    escribio = function(r) nrow(r$cuentas) == 2L && "0200" %in% r$cuentas$cuenta),
  cuenta_delete = list(servidor = settings_cuentas_observer, permiso = "can_manage_bancos",
    clics = function(session, output) {
      session$setInputs(`cuentas_t-accion` = list(accion = "editar", ids = "c1", nonce = 1))     # ✎ on c1
      session$setInputs(cuenta_delete = 1)
    },
    escribio = function(r) identical(r$cuentas$activa, FALSE)),
  save_interco_v2 = list(servidor = settings_observers, permiso = "can_manage_interco",
    clics = function(session, output) {
      session$setInputs(ic_ar_NTS_1 = "1027", ic_ar_NTS_2 = "3003")
      session$setInputs(save_interco_v2 = 1)
    },
    escribio = function(r) "3003" %in% r$registro$companies$NTS$ar),
  ic_scan_apply = list(servidor = settings_observers, permiso = "can_manage_interco",
    clics = function(session, output) {
      session$setInputs(scan_ic_btn = 1)
      session$setInputs(ic_scan_tbl_rows_selected = 2)   # C2001, the one not yet IC
      session$setInputs(ic_scan_apply = 1)
    },
    escribio = function(r) "2001" %in% r$registro$companies$NTS$ar),
  bp_apply_ic_btn = list(servidor = settings_observers, permiso = "can_manage_interco",
    clics = function(session, output) {
      session$setInputs(bp_refresh_btn = 1)
      invisible(output$tbl_bp_sap)                       # draws the table: sets the sorted candidates
      session$setInputs(tbl_bp_sap_rows_selected = 2)
      session$setInputs(bp_apply_ic_btn = 1)
    },
    escribio = function(r) "2001" %in% r$registro$companies$NTS$ar)
)

for (g in names(GUARDADOS)) {
  G <- GUARDADOS[[g]]
  r <- correr(SIN[[G$permiso]], G$servidor, G$clics)
  ok(sprintf("%s sin %s: no escribe en S3 (%d escrituras)", g, G$permiso, r$escrituras),
     r$escrituras == 0L && identical(r$bancos, BANCOS0) && identical(r$cuentas, CTAS0) && identical(r$registro, REG0))
  r <- correr(CON, G$servidor, G$clics)
  ok(sprintf("%s con %s: escribe (%d escrituras)", g, G$permiso, r$escrituras),
     r$escrituras >= 1L && isTRUE(G$escribio(r)))
}

# ── Without the permission the buttons that write are not drawn ────────────
html <- function(x) paste(as.character(x), collapse = "")
tiene <- function(h, ids) vapply(ids, function(i) grepl(sprintf('id="%s"', i), h, fixed = TRUE), logical(1))

ok("Cuentas de Empresa sin permiso: sin “+ Banco” ni “+ Cuenta”; las tablas sí",
   { h <- html(.cuentas_panel_ui(CMAP, editar = FALSE))
     !any(tiene(h, c("cta_new_banco", "cta_new_cuenta"))) && all(tiene(h, c("bancos_cat_t-hdt", "cuentas_t-hdt"))) })
ok("Cuentas de Empresa con permiso: “+ Banco” y “+ Cuenta”",
   all(tiene(html(.cuentas_panel_ui(CMAP)), c("cta_new_banco", "cta_new_cuenta"))))
ok("formulario de banco sin permiso: solo lectura (sin Guardar ni Eliminar)",
   !any(tiene(html(.banco_form_ui(BANCOS0, editar = FALSE)), c("banco_save", "banco_delete"))))
ok("formulario de banco con permiso: Guardar y Eliminar",
   all(tiene(html(.banco_form_ui(BANCOS0)), c("banco_save", "banco_delete"))))
ok("formulario de cuenta sin permiso: solo lectura (sin Guardar ni Desactivar)",
   !any(tiene(html(.cuenta_form_ui(CTAS0, BANCOS0, cmap = CMAP, editar = FALSE)), c("cuenta_save", "cuenta_delete"))))
ok("formulario de cuenta con permiso: Guardar y Desactivar",
   all(tiene(html(.cuenta_form_ui(CTAS0, BANCOS0, cmap = CMAP)), c("cuenta_save", "cuenta_delete"))))

CANDS <- scan_ic_candidates(SNAP_AR, NULL, REG0, CMAP)
ok("Business Partners sin can_manage_interco: sin “Aplicar como IC”; la tabla sí",
   { h <- html(isolate(.bp_panel_ui(nuevo_shared(SIN$can_manage_interco), CANDS)))
     !tiene(h, "bp_apply_ic_btn") && grepl("tbl_bp_sap", h, fixed = TRUE) })
ok("Business Partners con can_manage_interco: “Aplicar como IC”",
   tiene(html(isolate(.bp_panel_ui(nuevo_shared(CON), CANDS))), "bp_apply_ic_btn"))

panel <- function(u, boton, salida = "settings_panel") {
  h <- NULL
  correr(u, settings_observers, function(session, output) {
    do.call(session$setInputs, stats::setNames(list(1), boton))
    h <<- paste(html(output$settings_panel$html), if (salida != "settings_panel") html(output[[salida]]$html))
  })
  h
}
# The IC panel: the saved registry (engine table) and, below, the editor.
ok("panel Intercompany sin can_manage_interco: sin Guardar ni Escáner; el registro guardado sí",
   { h <- panel(SIN$can_manage_interco, "stg_btn_interco", "ic_editor")
     !any(tiene(h, c("save_interco_v2", "scan_ic_btn"))) && grepl("ic_registro_t", h, fixed = TRUE) })
ok("panel Intercompany con can_manage_interco: Guardar y Escáner",
   all(tiene(panel(CON, "stg_btn_interco", "ic_editor"), c("save_interco_v2", "scan_ic_btn"))))
ok("panel Cuentas de Empresa abierto por un cliente: sin “+ Banco”",
   !tiene(panel(SIN$can_manage_bancos, "stg_btn_cuentas"), "cta_new_banco"))
ok("panel Cuentas de Empresa con can_manage_bancos: “+ Banco”",
   tiene(panel(CON, "stg_btn_cuentas"), "cta_new_banco"))

modal_escaner <- function(u) {
  correr(u, settings_observers, function(session, output) session$setInputs(scan_ic_btn = 1))
  html(MODALES[[length(MODALES)]])
}
ok("escáner sin can_manage_interco: sin “Aplicar selección”", !tiene(modal_escaner(SIN$can_manage_interco), "ic_scan_apply"))
ok("escáner con can_manage_interco: “Aplicar selección”",   tiene(modal_escaner(CON), "ic_scan_apply"))

# ── Static: each handler's first line is the check ───────────────────────────
primera <- function(archivo, input) {
  src <- readLines(archivo, encoding = "UTF-8")
  i <- which(trimws(src) == sprintf("observeEvent(input$%s, {", input))
  if (length(i) != 1L) return(NA_character_)
  trimws(src[i + 1L])
}
for (g in names(GUARDADOS)) {
  archivo <- if (GUARDADOS[[g]]$permiso == "can_manage_bancos") "R/settings/settings_cuentas.R" else "R/settings/settings_hub.R"
  esperado <- sprintf('if (!puede_o_avisa(shared, "%s")) return()', GUARDADOS[[g]]$permiso)
  ok(sprintf("%s empieza revisando %s", g, GUARDADOS[[g]]$permiso), identical(primera(archivo, g), esperado))
}

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
