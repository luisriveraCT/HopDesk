# =============================================================================
# tests/test_t120_asistente_politicas.R
# T120, Configuración and the Pasivos wizard:
#   A. Configuración's editor is the same: its form, sub-forms and preview
#      render exactly as c119d720's (the ids it always had); with the wizard's
#      ns every id is pwiz_pol_*, none of Configuración's.
#   B. The shared server side (.politica_editor_logica) saves through
#      politica_guardar() with Configuración's ids AND with the wizard's; a
#      user without can_manage_policies writes nothing (server side).
#   C. Deleting a policy that liabilities use is refused, listing them; a
#      deleted liability does not count; a failed read refuses too.
#   D. The wizard: only catalog ids are kept; changing a liability's policies
#      needs can_manage_pasivos (server side); the order survives the draft.
#   E. Editing a liability to add a policy moves its provisional provisions
#      (the edit's reconcile), and never a converted one.
# Run from project root:  Rscript --vanilla tests/test_t120_asistente_politicas.R
# =============================================================================
cat("=== test_t120_asistente_politicas ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid) })
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/policy_engine.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/zona.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/ui_components.R", "R/adjuntos_pasivo_ui.R", "R/settings/settings_policies.R",
            "R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R", "R/pasivos_capabilities.R",
            "R/pasivos_audit.R", "R/forecasting_resolve.R", "R/pasivos_engine.R", "R/pasivos_deuda.R",
            "R/pasivos_wizard_validate.R", "R/pasivos_wizard_module.R", "R/pasivos_wizard_steps.R"))
  suppressMessages(source(f, encoding = "UTF-8"))
.i18n_dir <- file.path(getwd(), "www", "i18n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L } else { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
LOGS <- character(); AVISOS <- character(); MODAL <- NULL
log_action <- function(..., action = NULL) { LOGS <<- c(LOGS, action %||% "?"); invisible(NULL) }
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, type); invisible("id") }
bump_sync_version <- function(...) invisible(NULL)
showModal <- function(ui, ...) { MODAL <<- paste(as.character(ui), collapse = ""); invisible(NULL) }
removeModal <- function(...) invisible(NULL)
show_settings_modal <- function(...) invisible(NULL)
hoy_cliente <- function(...) as.Date("2026-11-26")
forecasting_get_estimate <- function(...) NA_real_
pasivos_log_audit <- function(...) invisible(NULL)
html <- function(x) paste(as.character(x), collapse = "")

cat("== A. Configuración's editor is the same ==\n")
viejo <- new.env(parent = globalenv())
src <- system2("git", c("show", "c119d720:R/settings/settings_policies.R"), stdout = TRUE)
eval(parse(text = src, keep.source = FALSE), viejo)
pol <- list(name = "Viernes", type = "month_days", roll_direction = "backward", params = list(days = c(10L, 25L)))
ok("A.1 the editor form renders exactly as before (new and edit)",
   identical(html(.policy_editor_form()), html(viejo$.policy_editor_form())) &&
     identical(html(.policy_editor_form(pol, FALSE)), html(viejo$.policy_editor_form(pol, FALSE))))
tipos <- c("weekdays", "skip_holidays", "last_day", "month_days", "offset_days")
ok("A.2 every type's sub-form renders exactly as before",
   all(vapply(tipos, function(t) identical(html(.policy_params_ui(t)), html(viejo$.policy_params_ui(t))), logical(1))))
ok("A.3 the preview calendar renders exactly as before",
   identical(html(.policy_cal_preview_render("weekdays", list(days = 1:5), "forward", list(), 0L, "2026-12-03")),
             html(viejo$.policy_cal_preview_render("weekdays", list(days = 1:5), "forward", list(), 0L, "2026-12-03"))))
wiz_html <- paste(html(.policy_editor_form(NULL, TRUE, ns = .pwiz_pol_ns)),
                  vapply(tipos, function(t) html(.policy_params_ui(t, ns = .pwiz_pol_ns)), ""),
                  html(.policy_cal_preview_render("weekdays", list(days = 1:5), "forward", list(), 0L, character(), ns = .pwiz_pol_ns)),
                  collapse = "")
ids_wiz <- regmatches(wiz_html, gregexpr("(id|for|name|data-for)=\"[^\"]*pol_[^\"]*\"", wiz_html))[[1]]
js_wiz  <- regmatches(wiz_html, gregexpr("setInputValue[(](&#39;|')[A-Za-z_]+", wiz_html))[[1]]   # ' escaped in onclick
ok("A.4 with the wizard's ns every id and every JS input is pwiz_pol_* -- none of Configuración's",
   length(ids_wiz) > 8L && all(grepl("pwiz_pol_", ids_wiz)) && length(js_wiz) >= 2L && all(grepl("pwiz_pol_", js_wiz)))

cat("== B. the shared server side ==\n")
CID <- "acme"
k <- function(key) paste0(CID, "/", key)
cat0 <- tibble::tibble(id = "p1", name = "Viernes", type = "weekdays", params = list(list(days = 5L)),
                       roll_direction = "forward", created_by = "x", created_at = Sys.time(), updated_at = Sys.time())
admin <- list(user = "ana", tier = "admin", permisos = permisos_resueltos("admin"))
sin_pol <- list(user = "beto", tier = "admin", permisos = stats::setNames(list(FALSE), "can_manage_policies"))
sesion <- function(u = admin) list(
  usuario = reactiveVal(u), current_user = function() u$user,
  current_user_info = reactive(list(username = u$user, tier = u$tier)),
  effective_client_id = function() CID, home_client_id = function() CID,
  ctx_no_cargadas = reactiveVal(character()), policy_catalog_db = reactiveVal(cat0),
  partner_policies_db = reactiveVal(NULL), holiday_overrides_db = reactiveVal(NULL),
  pasivos_provisions_db = reactiveVal(NULL), sap_data = reactiveVal(NULL))
sembrar <- function(liabs = NULL) {
  s3_memoria_reset(); LOGS <<- character(); AVISOS <<- character(); MODAL <<- NULL
  assign(k(S3_KEYS$policy_catalog), cat0, envir = .mem)
  if (!is.null(liabs)) assign(k(S3_KEYS$pasivos_liabilities), liabs, envir = .mem)
}
guardado <- NULL
editor <- function(shared, ns, pasos) testServer(function(input, output, session) {
  m <- reactiveVal("new"); e <- reactiveVal(NULL); o <- reactiveVal(0L); s <- reactiveVal(character())
  .politica_editor_logica(input, output, session, shared, ns = ns, pol_mode = m, pol_editing = e,
                          pol_cal_offset = o, pol_cal_selected = s, al_guardar = function(res) guardado <<- res)
}, { session$flushReact(); pasos(session) })
sembrar(); guardado <- NULL
editor(sesion(), identity, function(s) s$setInputs(pol_name = "Lunes", pol_type = "weekdays", pol_param_days = "1", pol_btn_save = 1))
c1 <- get(k(S3_KEYS$policy_catalog), envir = .mem)
ok("B.1 Configuración's ids (pol_name, pol_btn_save...): the new policy is written and handed back",
   "Lunes" %in% c1$name && identical(guardado$pid, c1$id[c1$name == "Lunes"]) && "crear_politica" %in% LOGS)
sembrar(); guardado <- NULL
editor(sesion(), .pwiz_pol_ns, function(s) s$setInputs(pwiz_pol_name = "Quincena", pwiz_pol_type = "month_days",
                                                       pwiz_pol_param_month_days = c("15", "30"), pwiz_pol_btn_save = 1))
c2 <- get(k(S3_KEYS$policy_catalog), envir = .mem)
ok("B.2 the wizard's ids (pwiz_pol_*): the same save, and the new id comes back for the liability",
   "Quincena" %in% c2$name && identical(guardado$pid, c2$id[c2$name == "Quincena"]) &&
     identical(c2$params[[which(c2$name == "Quincena")]]$days, c(15L, 30L)))
sembrar(); guardado <- NULL
n0 <- .mem_escrituras
r <- isolate(politica_guardar(sesion(sin_pol), mode = "new", nombre = "X", tipo = "weekdays", params = list(), roll = "forward"))
ok("B.3 without can_manage_policies: refused on the server, nothing written", identical(r$ok, FALSE) && .mem_escrituras == n0)

cat("== C. deleting a policy that liabilities use ==\n")
liabs <- data.frame(id = c("l1", "l2", "l3"), nombre = c("Renta", "Préstamo", "Viejo"),
                    policy_ids = c("p1", "p9,p1", "p1"), estado = c("active", "active", "deleted"), stringsAsFactors = FALSE)
sembrar(liabs)
ok("C.1 the liabilities that use it, by name; a deleted one does not count",
   identical(politica_pasivos_que_usan(CID, "p1"), c("Préstamo", "Renta")) && !length(politica_pasivos_que_usan(CID, "p9x")))
borrar <- function(s) {
  s$setInputs(pol_action = list(action = "delete", id = "p1", name = "Viernes", nonce = runif(1)))
  s$setInputs(pol_delete_confirm = 1)
}
testServer(function(input, output, session) settings_policies_observer(input, output, session, sesion()),
           { session$flushReact(); borrar(session) })
ok("C.2 Configuración refuses to delete it and lists them; the catalog keeps it",
   "p1" %in% get(k(S3_KEYS$policy_catalog), envir = .mem)$id && grepl("Renta", MODAL) && grepl("Préstamo", MODAL) &&
     !grepl("Viejo", MODAL))
sembrar(liabs[liabs$id == "l3", ])
testServer(function(input, output, session) settings_policies_observer(input, output, session, sesion()),
           { session$flushReact(); borrar(session) })
ok("C.3 contraprueba: used only by a deleted liability, it is deleted", !"p1" %in% get(k(S3_KEYS$policy_catalog), envir = .mem)$id)
sembrar(liabs)
viejo_leer <- .s3_leer_para_escribir
.s3_leer_para_escribir <- function(key, client_id) if (grepl("liabilit", key)) stop("[S3] could not read") else viejo_leer(key, client_id)
testServer(function(input, output, session) settings_policies_observer(input, output, session, sesion()),
           { session$flushReact(); borrar(session) })
.s3_leer_para_escribir <- viejo_leer
ok("C.4 a failed read of the liabilities refuses too (never deletes blind)",
   "p1" %in% get(k(S3_KEYS$policy_catalog), envir = .mem)$id && "error" %in% AVISOS)

# Another session assigns it between the click and the confirm: the confirm
# checks S3 again.
sembrar(liabs[liabs$id == "l3", ]); MODAL <- NULL
testServer(function(input, output, session) settings_policies_observer(input, output, session, sesion()), {
  session$flushReact()
  session$setInputs(pol_action = list(action = "delete", id = "p1", name = "Viernes", nonce = runif(1)))
  assign(k(S3_KEYS$pasivos_liabilities), liabs, envir = .mem)        # meanwhile, in another session
  session$setInputs(pol_delete_confirm = 1)
})
ok("C.5 assigned meanwhile by another session: the confirm refuses too (checked again on S3)",
   "p1" %in% get(k(S3_KEYS$policy_catalog), envir = .mem)$id && grepl("Renta", MODAL %||% ""))

cat("== D. the wizard ==\n")
mk_wiz <- function(ids, mode = "create", target = NULL) {
  w <- reactiveValues(draft = list(policy_ids = ids), mode = mode, edit_target_id = target); w }
sh <- function(u = admin, liabs = NULL) c(sesion(u), list(pasivos_liabilities_db = reactiveVal(liabs)))
ok("D.1 only the catalog's ids are kept, in the draft's order", isolate({
  w <- mk_wiz(c("p1", "nope")); .wiz_politicas_autoriza(w, sh()) && identical(w$draft$policy_ids, "p1") }))
sin_pas <- list(user = "c", tier = "admin", permisos = stats::setNames(list(FALSE), "can_manage_pasivos"))
ok("D.2 assigning without can_manage_pasivos is refused on the server", isolate(!.wiz_politicas_autoriza(mk_wiz("p1"), sh(sin_pas))))
lib <- data.frame(id = "L", policy_ids = "p1", stringsAsFactors = FALSE)
ok("D.3 ...but unchanged policies (an edit of something else) are not a change", isolate(
  .wiz_politicas_autoriza(mk_wiz("p1", "edit", "L"), sh(sin_pas, lib)) && !.wiz_politicas_autoriza(mk_wiz(character(), "edit", "L"), sh(sin_pas, lib))))
dr <- list(categoria = "regular", nombre = "Renta", empresa = "NTS", parte = "X", moneda_pago = "MXN",
           recurrence_type = "monthly_day", recurrence_params = list(day = 1L), fecha_inicio = as.Date("2027-01-01"),
           amount_default = 100, policy_ids = c("p_fest", "p_hab"))
L <- pasivos_wizard_draft_to_liability(dr, mode = "create", user = "t")
ok("D.4 the draft's order survives the liability and back", identical(L$policy_ids, "p_fest,p_hab") &&
     identical(pasivos_liability_to_draft(L)$policy_ids, c("p_fest", "p_hab")))
ok("D.5 the policies block is in the wizard's modal, outside the step's redraw (T120 review)",
   grepl("pwiz_politicas_ui", html(.pwiz_modal())) && !grepl("pwiz_politicas_ui", html(pasivos_step_ui_revision(dr))))

cat("== E. editing a liability to add a policy ==\n")
catp <- tibble::tibble(id = c("p_hab", "p_fest"), name = c("H", "F"), type = c("weekdays", "skip_holidays"),
                       params = list(list(days = 1:5), list(country = "MX")), roll_direction = "forward")
L0 <- pasivos_wizard_draft_to_liability(modifyList(dr, list(policy_ids = character())), mode = "create", user = "t")
ex <- pasivos_generate_provisions(L0, window_start = as.Date("2026-11-26"), window_end = as.Date("2027-06-30"), client_id = "c",
                                  policies_for_liability = pasivos_politicas_de(L0, catp))
ex$estado <- "provisional"; ex$estado[as.Date(ex$fecha_calculada) == as.Date("2027-02-01")] <- "converted"
L1 <- L0; L1$policy_ids <- "p_fest,p_hab"
g1 <- pasivos_generate_provisions(L1, window_start = as.Date("2026-11-26"), window_end = as.Date("2027-06-30"), client_id = "c",
                                  policies_for_liability = pasivos_politicas_de(L1, catp))
rr <- pasivos_reconciliar_o_conservar(ex, g1, mode = "regenerate", liability = L1, liability_antes = L0)
todo <- dplyr::bind_rows(rr$keep, rr$conflicts, rr$update, rr$insert)
fe <- function(x) as.Date(todo$fecha_efectiva[as.Date(todo$fecha_calculada) == as.Date(x)])
ok("E.1 the edit moves a provisional one (1 Jan -> 4 Jan) and never the converted one (1 Feb stays)",
   identical(fe("2027-01-01"), as.Date("2027-01-04")) && identical(fe("2027-02-01"), as.Date("2027-02-01")))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
