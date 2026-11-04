# =============================================================================
# tests/test_ajustes_permiso_b.R
# The other saves of Ajustes check their permission on the server (T16b):
#   - payment policies and their per-partner assignment and application
#     (settings_policies.R, settings_companies.R) -> can_manage_policies
#     (Dev, Admin and Finanzas; Luis, 2026-11-04); they decide the policy
#     payment dates, and Aplicar can prune the movements (save_moves);
#   - the Agenda sync (settings_sincro.R) -> can_manage_sync.
#
# On the real observers in testServer; every save_* is a recorder:
#   1. without the permission none of the 8 handlers saves anything;
#   2. with it, the ones a test can drive end to end save;
#   3. without it, the buttons that write are not drawn (reading still is);
#   4. static: each handler starts with puede_o_avisa().
# The 2 observers of settings_companies.R that save on their own (on
# sap_data / partner_policies_db) are not here: CM12a.
#
# Run from project root:  Rscript tests/test_ajustes_permiso_b.R
# =============================================================================
cat("=== test_ajustes_permiso_b ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

source("tests/helpers/s3_memoria.R")   # persistence.R (schemas) over an in-memory S3
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/policy_engine.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/settings/settings_policies.R", "R/settings/settings_companies.R", "R/settings/settings_sincro.R"))
  suppressMessages(source(f))

# Recorders and stand-ins (after the sources, so they win). puede() and
# puede_o_avisa() are the real ones.
GUARDADO <- character(0)
grabar <- function(nombre) { force(nombre); function(...) { GUARDADO <<- c(GUARDADO, nombre); invisible(NULL) } }
for (s in c("save_policy_catalog", "save_partner_policies", "save_policy_moves", "save_moves",
            "modificar_partner_policies", "modificar_policy_moves",   # CM12e: on S3's current table
            "save_pagar_hoy_sync", "save_pagar_hoy_user", "save_agenda_sync_config"))
  assign(s, grabar(s))
log_action <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
showNotification <- function(...) invisible("id")
showModal <- function(...) invisible(NULL)
removeModal <- function(...) invisible(NULL)
show_settings_modal <- function(...) invisible(NULL)
load_pagar_hoy_user <- function(...) tibble::tibble(id = "a1", Documento = "F1")

nivel <- function(t) list(user = t, tier = t, permisos = permisos_resueltos(t))
CON <- nivel("admin")
SIN <- list(can_manage_policies = nivel("analysis"), can_manage_sync = nivel("finance"))
stopifnot(puede(CON, "can_manage_policies"), puede(CON, "can_manage_sync"),
          !puede(SIN$can_manage_policies, "can_manage_policies"), !puede(SIN$can_manage_sync, "can_manage_sync"),
          puede(nivel("finance"), "can_manage_policies"))

CATALOGO <- tibble::tibble(id = c("p1", "p2"), name = c("Viernes", "Quincena"), type = "weekdays",
                           roll_direction = "forward", params = "{}")
SOCIOS   <- tibble::tibble(parte = "PROV A", policy_id = "p1", is_interco = FALSE)
MOVS     <- tibble::tibble(ledger = "AP", Empresa = "NTS", Moneda = "MXN", Documento = c("F1", "F2"),
                           Parte = c("PROV A", "PROV B"), FechaVenc_Politica = as.Date("2026-11-06"),
                           policy_ids = "p1", applied_by = "auto", applied_at = Sys.time())

correr <- function(u, clics) {
  GUARDADO <<- character(0)
  shared <- list(usuario = reactiveVal(u), current_user = function() u$user,
                 effective_client_id = function() "acme", home_client_id = function() "acme",
                 company_map = reactive(c(NTS = "Networks Trucking")),
                 sap_data = reactiveVal(NULL), policy_catalog_db = reactiveVal(CATALOGO),
                 partner_policies_db = reactiveVal(SOCIOS), policy_moves_db = reactiveVal(MOVS),
                 holiday_overrides_db = reactiveVal(NULL), moves_db = reactiveVal(NULL))
  testServer(function(input, output, session) {
    settings_policies_observer(input, output, session, shared)
    settings_companies_observer(input, output, session, shared)
    settings_sincro_observer(input, output, session, shared, pagar_hoy_db = reactiveVal(NULL))
  }, { session$flushReact(); clics(session, output) })
  GUARDADO
}

# ── The 8 handlers ───────────────────────────────────────────────────────────
HANDLERS <- list(
  pol_btn_save = list(permiso = "can_manage_policies", archivo = "settings_policies.R",
    clics = function(session, output) session$setInputs(pol_name = "Lunes", pol_btn_save = 1)),
  pol_assign_save = list(permiso = "can_manage_policies", archivo = "settings_policies.R",
    clics = function(session, output) {
      session$setInputs(pol_action = list(action = "assign", id = "p1", name = "Viernes", nonce = 1))
      session$setInputs(pol_assign_partes = "PROV B", pol_assign_save = 1)
    }),
  pol_delete_confirm = list(permiso = "can_manage_policies", archivo = "settings_policies.R",
    clics = function(session, output) {
      session$setInputs(pol_action = list(action = "delete", id = "p2", name = "Quincena", nonce = 1))
      session$setInputs(pol_delete_confirm = 1)
    },
    con = "save_policy_catalog"),
  cmp_btn_save = list(permiso = "can_manage_policies", archivo = "settings_companies.R",
    clics = function(session, output) session$setInputs(cmp_btn_save = 1)),
  cmp_apply_confirm = list(permiso = "can_manage_policies", archivo = "settings_companies.R",
    clics = function(session, output) session$setInputs(cmp_apply_scope = "new", cmp_apply_confirm = 1)),
  cmp_revert_confirm = list(permiso = "can_manage_policies", archivo = "settings_companies.R",
    clics = function(session, output) session$setInputs(cmp_revert_partes_sel = "PROV A", cmp_revert_confirm = 1),
    con = "modificar_policy_moves"),
  do_sincro_activar = list(permiso = "can_manage_sync", archivo = "settings_sincro.R",
    clics = function(session, output) session$setInputs(sincro_src_selected = list(user = "luis"), do_sincro_activar = 1),
    con = "save_pagar_hoy_sync"),
  do_sincro_desactivar = list(permiso = "can_manage_sync", archivo = "settings_sincro.R",
    clics = function(session, output) session$setInputs(do_sincro_desactivar = 1))
)

for (h in names(HANDLERS)) {
  H <- HANDLERS[[h]]
  g <- correr(SIN[[H$permiso]], H$clics)
  ok(sprintf("%s sin %s: no guarda nada (%s)", h, H$permiso, if (length(g)) paste(g, collapse = ", ") else "0"),
     !length(g))
  if (!is.null(H$con)) {
    g <- correr(CON, H$clics)
    ok(sprintf("%s con %s: guarda (%s)", h, H$permiso, paste(g, collapse = ", ")), H$con %in% g)
  }
}

# ── Without the permission the buttons that write are not drawn ────────────
html <- function(x) paste(as.character(x), collapse = "")
tiene <- function(h, ids) vapply(ids, function(i) grepl(sprintf('id="%s"', i), h, fixed = TRUE), logical(1))
con_permiso <- function(si, expr) {
  dom <- MockShinySession$new()
  dom$userData$puede_politicas <- function() si
  withReactiveDomain(dom, html(expr))
}
for (si in c(FALSE, TRUE)) {
  que <- if (si) "con" else "sin"
  h_reglas <- con_permiso(si, .policies_merged_panel_ui("reglas"))
  h_socios <- con_permiso(si, .policies_merged_panel_ui("socios"))
  h_lista  <- con_permiso(si, .policies_list_render(CATALOGO))
  h_editor <- con_permiso(si, .policy_editor_form(as.list(CATALOGO[1, ]), is_new = FALSE))
  h_asig   <- con_permiso(si, .policies_assign_form("p1", "Viernes", "weekdays", "PROV A", c("PROV A", "PROV B")))
  ok(sprintf("Políticas %s can_manage_policies: “Nueva política” %s", que, if (si) "sí" else "no"),
     tiene(h_reglas, "pol_btn_new") == si)
  ok(sprintf("Socios %s permiso: Agregar y Guardar cambios %s", que, if (si) "sí" else "no"),
     all(tiene(h_socios, c("cmp_btn_add", "cmp_btn_save")) == si))
  ok(sprintf("lista de políticas %s permiso: asignar y eliminar %s; editar (ver) siempre", que, if (si) "sí" else "no"),
     grepl("action:(&#39;|')assign", h_lista) == si && grepl("action:(&#39;|')delete", h_lista) == si &&
       grepl("action:(&#39;|')edit", h_lista))
  ok(sprintf("editor de política %s permiso: Guardar %s", que, if (si) "sí" else "no"),
     tiene(h_editor, "pol_btn_save") == si)
  ok(sprintf("asignar a socios %s permiso: Guardar asignaciones %s", que, if (si) "sí" else "no"),
     tiene(h_asig, "pol_assign_save") == si)
}

botones_socios <- function(u) {
  h <- NULL
  GUARDADO <<- character(0)
  shared <- list(usuario = reactiveVal(u), current_user = function() u$user,
                 effective_client_id = function() "acme", home_client_id = function() "acme",
                 company_map = reactive(c(NTS = "Networks Trucking")), sap_data = reactiveVal(NULL),
                 policy_catalog_db = reactiveVal(CATALOGO), partner_policies_db = reactiveVal(SOCIOS),
                 policy_moves_db = reactiveVal(MOVS), holiday_overrides_db = reactiveVal(NULL))
  testServer(function(input, output, session) {
    settings_policies_observer(input, output, session, shared)
    settings_companies_observer(input, output, session, shared)
  }, { session$flushReact(); h <<- paste(html(output$cmp_apply_btn_ui$html), html(output$cmp_revert_btn_ui$html)) })
  h
}
ok("Socios sin can_manage_policies: sin “Aplicar políticas” ni “Revertir”",
   !any(tiene(botones_socios(SIN$can_manage_policies), c("cmp_btn_apply", "cmp_btn_revert"))))
ok("Socios con can_manage_policies: “Aplicar políticas” y “Revertir”",
   all(tiene(botones_socios(CON), c("cmp_btn_apply", "cmp_btn_revert"))))

# ── Static: each handler's first line is the check ───────────────────────────
for (h in names(HANDLERS)) {
  src <- readLines(file.path("R/settings", HANDLERS[[h]]$archivo), encoding = "UTF-8")
  i <- which(trimws(src) == sprintf("observeEvent(input$%s, {", h))
  ok(sprintf("%s empieza revisando %s", h, HANDLERS[[h]]$permiso),
     length(i) == 1L && identical(trimws(src[i + 1L]), sprintf('if (!puede_o_avisa(shared, "%s")) return()', HANDLERS[[h]]$permiso)))
}

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
