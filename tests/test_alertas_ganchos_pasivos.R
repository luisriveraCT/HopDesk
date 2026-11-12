# =============================================================================
# tests/test_alertas_ganchos_pasivos.R
# Alertas del equipo (C) in Pasivos: the one-line hooks, as the Ejecutor 3
# wrote them (requisiciones/bitacora.md, "Pasivos (espera a CM21)").
#   S  structure: crear_provision once in me_save_as_provision and once in
#      ppm_save, after the save succeeded and after its audit; crear_pasivo
#      twice in .pasivos_wizard_save, after each liability.created audit,
#      only on create.
#   B  behaviour (the real R/ loaded as the app does, on the in-memory S3,
#      avisos_actividad() recorded): a save that works announces once; a
#      failed save, a missing date or an edit announce nothing; a liability
#      saved whose provisions failed is still announced (P5).
# Run from project root:  Rscript tests/test_alertas_ganchos_pasivos.R
# =============================================================================
cat("=== test_alertas_ganchos_pasivos ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
Sys.unsetenv("HOPDESK_PRODUCTION")
suppressWarnings(suppressMessages(shiny::loadSupport(".", renv = globalenv(), globalrenv = NULL)))
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── S: where the hooks are ─────────────────────────────────────────────────
ganchos <- function(f) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  i <- grep("avisos_actividad(", x, fixed = TRUE); i <- i[!grepl("^\\s*#", x[i])]
  data.frame(linea = i, accion = sub('^.*avisos_actividad\\([^,]+, "([a-z_]+)".*$', "\\1", x[i]), stringsAsFactors = FALSE)
}
antes_de <- function(x, patron, linea) { j <- grep(patron, x, fixed = TRUE); any(j < linea) }

pm <- readLines("R/pasivos_module.R", warn = FALSE, encoding = "UTF-8"); g <- ganchos("R/pasivos_module.R")
ini <- grep("observeEvent(input$me_save_as_provision", pm, fixed = TRUE)
ok("S pasivos_module: one crear_provision hook, inside me_save_as_provision",
   nrow(g) == 1L && g$accion == "crear_provision" && length(ini) == 1L && g$linea > ini)
ok("S pasivos_module: after `if (!save_ok) return()` and after the provision.generated audit",
   antes_de(pm[ini:g$linea], "if (!save_ok) return()", g$linea - ini + 1L) &&
     antes_de(pm[ini:g$linea], 'action_type = "provision.generated"', g$linea - ini + 1L))

pt <- readLines("R/pasivos_table_module.R", warn = FALSE, encoding = "UTF-8"); g <- ganchos("R/pasivos_table_module.R")
ini <- grep("observeEvent(input$ppm_save", pt, fixed = TRUE)
ok("S pasivos_table_module: one crear_provision hook, inside ppm_save, after `if (!ok) return()` and the audit",
   nrow(g) == 1L && g$accion == "crear_provision" && g$linea > ini &&
     antes_de(pt[ini:g$linea], "if (!ok) return()", g$linea - ini + 1L) &&
     antes_de(pt[ini:g$linea], '"provision.generated"', g$linea - ini + 1L))

pw <- readLines("R/pasivos_wizard_module.R", warn = FALSE, encoding = "UTF-8"); g <- ganchos("R/pasivos_wizard_module.R")
aud <- grep('"liability.created" else "liability.edited"', pw, fixed = TRUE)
ok("S wizard: two crear_pasivo hooks, only on create",
   nrow(g) == 2L && all(g$accion == "crear_pasivo") && all(grepl('if (wiz$mode == "create")', pw[g$linea], fixed = TRUE)))
ok("S wizard: each one after its own liability.created audit (provisions failed, and full success)",
   length(aud) == 2L && g$linea[1] > aud[1] && g$linea[1] < aud[2] && g$linea[2] > aud[2])

# ── B: when they fire ──────────────────────────────────────────────────────
G <- new.env()
avisos_actividad <- function(client_id, accion, usuario, params = list()) {
  G$hooks[[length(G$hooks) + 1L]] <- list(cid = client_id, accion = accion, usuario = usuario, params = params)
  invisible(TRUE)
}
has_capability <- function(user, capability, empresa = NULL) TRUE
pasivos_autoriza <- function(capability) TRUE
G$falla <- FALSE
save_original <- save_pasivos_provisions
save_pasivos_provisions <- function(...) { if (isTRUE(G$falla)) stop("simulated S3 failure"); save_original(...) }

CID <- "acme"
prov <- function(id, fecha = Sys.Date() + 10) {
  p <- .schema_pasivos_provision()[NA_integer_, ]
  p$id <- id; p$estado <- "provisional"; p$origin <- "manual"; p$empresa <- "NTS"; p$parte <- "Proveedor"
  p$moneda_pago <- "MXN"; p$amount_pago <- 100; p$fecha_calculada <- fecha; p$fecha_efectiva <- fecha
  p$reverted_count <- 0L
  p
}
nuevo_shared <- function() list(
  effective_client_id = function() CID, current_user = reactiveVal("tester"),
  company_map = reactive(c(NTS = "Networks Transport")), empresa_sel = reactiveVal("Networks Transport"),
  pasivos_provisions_db = reactiveVal(prov("p1")), pasivos_liabilities_db = reactiveVal(.schema_pasivos_liability()),
  manual_inv = reactiveVal(.schema_manual()), pagar_hoy_db = reactiveVal(NULL), holiday_overrides_db = function() list(),
  bancos_confirmados = reactiveVal(NULL), visible_initials = reactiveVal(NULL),
  proveedores_db = reactiveVal(NULL), suppress_ledger_prov_refresh = function(...) invisible(NULL))
inicio <- function(falla = FALSE) {
  s3_memoria_reset(); G$hooks <- list(); G$falla <- falla
  assign(paste0(CID, "/", S3_KEYS$pasivos_provisions), prov("p1"), envir = .mem)
  invisible(suppressMessages(save_pasivos_liabilities(.schema_pasivos_liability(), client_id = CID)))
  G$hooks <- list()
}
acciones <- function() vapply(G$hooks, function(h) h$accion, "")
quieto <- function(expr) suppressWarnings(suppressMessages(expr))

# B1-B3: "Guardar como provisión" (pasivos_module.R)
guardar_como_provision <- function(fecha = Sys.Date() + 3, falla = FALSE) {
  inicio(falla); sh <- nuevo_shared()
  quieto(testServer(function(input, output, session) setup_pasivos_module(input, output, session, sh), {
    session$setInputs(me_fecha = fecha, me_empresa = "NTS", me_parte = "Proveedor", me_codigo = "", me_moneda = "MXN",
                      me_importe = 50, me_documento = "", me_referencia = "", me_notas = "")
    session$setInputs(me_save_as_provision = 1)
  }))
}
guardar_como_provision()
ok("B1 Guardar como provisión saved: one crear_provision, with the client, the user and the empresa",
   identical(acciones(), "crear_provision") && identical(G$hooks[[1]]$cid, CID) &&
     identical(G$hooks[[1]]$usuario, "tester") && identical(G$hooks[[1]]$params$cuerpo, "NTS"))
guardar_como_provision(falla = TRUE)
ok("B2 Guardar como provisión, failed save: no aviso", !length(G$hooks))
guardar_como_provision(fecha = as.Date(NA))
ok("B3 Guardar como provisión, no date (CM22): no aviso", !length(G$hooks))

# B4-B5: "+ Provisión manual" (pasivos_table_module.R)
mas_provision <- function(falla = FALSE) {
  inicio(falla); sh <- nuevo_shared()
  quieto(testServer(pasivos_table_module_server, args = list(shared = sh), {
    session$flushReact(); G$hooks <- list()
    session$setInputs(btn_add_provision_manual = 1)
    session$setInputs(ppm_empresa = "Networks Transport", ppm_moneda = "MXN", ppm_parte = "P", ppm_codigo = "",
                      ppm_importe = 10, ppm_fecha = Sys.Date() + 4, ppm_documento = "", ppm_referencia = "", ppm_notas = "")
    session$setInputs(ppm_save = 1)
    G$hooks_ppm <- G$hooks   # read inside the session (its ending clears what was noted)
  }))
}
mas_provision()
ok("B4 + Provisión manual saved: one crear_provision with the empresa",
   identical(vapply(G$hooks_ppm, function(h) h$accion, ""), "crear_provision") &&
     identical(G$hooks_ppm[[1]]$params$cuerpo, "Networks Transport"))
mas_provision(falla = TRUE)
ok("B5 + Provisión manual, failed save: no aviso", !length(G$hooks_ppm))

# B6-B8: the wizard (pasivos_wizard_module.R)
borrador <- function() list(categoria = "regular", nombre = "Renta oficina", empresa = "NTS", parte = "Arrendadora",
                            moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
                            fecha_inicio = Sys.Date(), amount_default = 100)
asistente <- function(mode = "create", falla_provs = FALSE) {
  inicio(); sh <- nuevo_shared()
  if (mode == "edit") {
    l <- pasivos_wizard_draft_to_liability(borrador(), mode = "create", user = "ana"); l$id <- "L1"
    quieto(save_pasivos_liabilities(l, client_id = CID)); sh$pasivos_liabilities_db(l)
    d <- pasivos_liability_to_draft(l); d$amount_default <- 120
  } else d <- borrador()
  wiz <- list(mode = mode, edit_target_id = if (mode == "edit") "L1", draft = d)
  G$hooks <- list(); G$falla <- falla_provs
  quieto(testServer(function(input, output, session) G$res <- .pasivos_wizard_save(wiz, sh, session, "beto"), {}))
}
asistente()
ok("B6 the wizard creates a liability: one crear_pasivo, named after it",
   isTRUE(G$res$ok) && identical(acciones(), "crear_pasivo") && identical(G$hooks[[1]]$params$cuerpo, "Renta oficina") &&
     identical(G$hooks[[1]]$usuario, "beto"))
asistente(mode = "edit")
ok("B7 the wizard edits a liability: no aviso", isTRUE(G$res$ok) && !length(G$hooks))
asistente(falla_provs = TRUE)
ok("B8 created, but its provisions failed: the liability is saved, so it is announced once (P5)",
   identical(acciones(), "crear_pasivo"))

s3_memoria_restaurar()
cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
