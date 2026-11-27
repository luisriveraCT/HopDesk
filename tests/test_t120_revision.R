# =============================================================================
# tests/test_t120_revision.R
# T120, the Coordinador's independent review -- each point with its
# contraprueba on 331e6146 (the T120 before it):
#   1. The policy selector is outside the step's redraw: a selection stays
#      after Revisión is drawn again, and opening liability B after A shows
#      B's policies (never A's from the cache).
#   2. A catalog that did not load is not an empty one: with policies
#      involved nothing is saved (the liability keeps its policies; said).
#   3. The refresh never moves a provision with a live payment, even one
#      still "provisional".
#   4. The holidays cover the years the provisions span (2029 too).
#   5. A backward roll never puts a future payment in the past (the next
#      valid day from today); a re-placed provision takes its document and
#      exchange rate from the generator, as the wizard's save does.
# Run from project root:  Rscript --vanilla tests/test_t120_revision.R
# =============================================================================
cat("=== test_t120_revision ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid) })
source("tests/helpers/s3_memoria.R")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
source("R/monedas.R"); source("R/zona.R")
ARCHIVOS <- c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R", "R/pasivos_capabilities.R",
              "R/pasivos_audit.R", "R/forecasting_resolve.R", "R/policy_engine.R", "R/pasivos_engine.R",
              "R/pasivos_wizard_validate.R", "R/pasivos_wizard_steps.R", "R/pasivos_wizard_module.R",
              "R/adjuntos_s3.R", "R/adjuntos_registro.R", "R/adjuntos_almacen.R", "R/adjuntos_documentos.R",
              "R/adjuntos_cfdi.R", "R/adjuntos_pasivo_ui.R", "R/pasivos_edit_confirm_module.R", "R/i18n.R",
              "R/pasivos_table_pivot.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/ui_components.R", "R/tier_registry.R", "R/permisos.R", "R/settings/settings_policies.R")
for (f in ARCHIVOS) suppressMessages(source(f, encoding = "UTF-8"))
for (e in parse("R/global.R", keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && identical(e[[2]], as.name("strip_accents"))) eval(e)
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
bump_sync_version <- function(...) invisible(NULL)
avisos_actividad <- function(...) invisible(TRUE)
pasivos_log_audit <- function(...) invisible(NULL)
pasivos_edit_confirm_open <- function(...) invisible(NULL)
has_capability <- function(...) TRUE
pasivos_autoriza <- function(...) TRUE
forecasting_get_estimate <- function(...) NA_real_
AVISOS <- character()
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, paste(type, paste(as.character(ui), collapse = ""))); invisible("id") }
.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L } else { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
html <- function(x) if (is.null(x)) "" else paste(as.character(if (is.list(x) && !is.null(x$html)) x$html else x), collapse = "")
seleccion <- function(h) {   # the selectize's selected ids, as the browser draws them
  m <- regmatches(h, regexpr('<select[^>]*id="pwiz_politicas".*?</select>', h))
  if (!length(m)) return(NA_character_)
  sort(sub('.*value="([^"]*)".*', "\\1", regmatches(m, gregexpr('<option value="[^"]*" selected', m))[[1]]))
}
viejo <- function(archivo) {   # 331e6146's version of a file, sourced over the current one
  src <- system2("git", c("show", paste0("331e6146:", archivo)), stdout = TRUE)
  f <- tempfile(fileext = ".R"); writeLines(src, f, useBytes = TRUE); f
}
CID <- "acme"; HOY <- as.Date("2026-11-26")
hoy_en <- function(...) HOY; hoy_cliente <- function(...) HOY
catalogo <- tibble::tibble(id = c("p1", "p2", "p_fest", "p_menos5", "p_hab"), name = c("Viernes", "Quincena", "Festivos MX", "Cinco antes", "Hábiles"),
                           type = c("weekdays", "month_days", "skip_holidays", "offset_days", "weekdays"),
                           params = list(list(days = 5L), list(days = c(15L, 30L)), list(country = "MX"), list(n = -5L, unit = "naturales"), list(days = 1:5)),
                           roll_direction = "forward",
                           created_by = "x", created_at = Sys.time(), updated_at = Sys.time())
renta <- function(nombre, pol, dia = 1L, extra = list()) {
  l <- pasivos_wizard_draft_to_liability(modifyList(list(categoria = "regular", nombre = nombre, empresa = "NG", parte = "X",
    moneda_pago = "USD", recurrence_type = "monthly_day", recurrence_params = list(day = dia), fecha_inicio = as.Date("2027-01-01"),
    amount_default = 100), extra), mode = "create", user = "t")
  l$policy_ids <- pol; l$estado <- "active"; l
}
A <- renta("Renta A", "p1"); B <- renta("Renta B", "p2")
admin <- list(user = "luis", tier = "admin", permisos = permisos_resueltos("admin"))
sesion <- function(cat = catalogo) list(
  pasivos_liabilities_db = reactiveVal(dplyr::bind_rows(A, B)), pasivos_provisions_db = reactiveVal(.schema_pasivos_provision()),
  effective_client_id = function() CID, suppress_ledger_prov_refresh = function(...) invisible(NULL),
  company_map = function() list(NG = "Nuevo Grupo"), holiday_overrides_db = function() NULL, proveedores_db = function() NULL,
  moneda_base_cliente = function() list(moneda = "USD"), current_user = function() "luis",
  usuario = reactiveVal(admin), policy_catalog_db = reactiveVal(cat), ctx_no_cargadas = reactiveVal(character()))

cat("== 1. the selector outside the redraw ==\n")
punto1 <- function() {
  r <- list()
  testServer(function(input, output, session) setup_pasivos_wizard(input, output, session, sesion()), {
    session$flushReact()
    session$setInputs(pasivos_edit_liability = A$id)
    r$abre_a <<- seleccion(html(output$pwiz_politicas_ui))
    session$setInputs(pwiz_politicas = c("p1", "p2"))                         # the person adds Quincena
    html(output$pwiz_step_content)                                            # Revisión drawn again
    r$sin_redibujo <<- seleccion(html(output$pwiz_politicas_ui))
    session$setInputs(pwiz_goto_step = "recurrencia"); html(output$pwiz_step_content)
    r$paso_fuera <<- html(output$pwiz_politicas_ui)
    session$setInputs(pwiz_amount_default = 100, pwiz_fecha_inicio = as.Date("2027-01-01"), pwiz_periodicidad = "monthly_day", pwiz_rp_day = 1L, pwiz_next = 1); html(output$pwiz_step_content)   # forward again to Revisión
    r$tras_redibujo <<- seleccion(html(output$pwiz_politicas_ui))
    session$setInputs(pasivos_edit_liability = B$id)                          # then B
    r$abre_b <<- seleccion(html(output$pwiz_politicas_ui))
  })
  r
}
r <- punto1()
ok(identical(r$abre_a, "p1"), "1.1 A opens with its policy selected")
ok(identical(r$sin_redibujo, "p1"),
   "1.2a choosing does not draw the selector again (Revisión's redraw no longer touches it: the browser keeps what was chosen)")
ok(!nzchar(r$paso_fuera), "1.2b on another step the selector is not shown")
ok(identical(r$tras_redibujo, c("p1", "p2")), "1.2 back on Revisión (drawn again), the selection is the one chosen")
ok(identical(r$abre_b, "p2"), "1.3 B opened after A shows B's policies, not A's")
for (f in c("R/pasivos_wizard_module.R", "R/pasivos_wizard_steps.R")) suppressMessages(source(viejo(f), encoding = "UTF-8"))
r0 <- tryCatch(punto1(), error = function(e) list())
ok(!identical(r0$tras_redibujo, c("p1", "p2")) || !identical(r0$abre_b, "p2"),
   "1.4 contraprueba: 331e6146 lost the selection or showed A's policies on B")
for (f in c("R/pasivos_wizard_module.R", "R/pasivos_wizard_steps.R")) suppressMessages(source(f, encoding = "UTF-8"))

cat("== 2. a catalog that did not load ==\n")
wz <- function(ids) reactiveValues(draft = list(policy_ids = ids), mode = "edit", edit_target_id = A$id)
s_null <- sesion(NULL)
AVISOS <- character()
w <- wz(c("p1", "p2"))
res <- isolate(.wiz_politicas_autoriza(w, s_null))
cuerpo <- paste(deparse(.wiz_politicas_autoriza), collapse = " ")
ok(identical(res, FALSE) && grepl("tablas.pasivos.politicas.sin_catalogo", cuerpo, fixed = TRUE),
   "2.1 no catalog and policies involved: nothing is saved, and the person is told (sin_catalogo)")
ok(identical(isolate(w$draft$policy_ids), c("p1", "p2")), "2.2 ...the draft is not emptied in silence")
s_jump <- sesion(); s_jump$ctx_no_cargadas(c("policy_catalog"))
ok(identical(isolate(.wiz_politicas_autoriza(wz("p1"), s_jump)), FALSE), "2.3 a jump that could not read the catalog: the same")
ok(isTRUE(isolate(.wiz_politicas_autoriza(reactiveValues(draft = list(policy_ids = character()), mode = "create", edit_target_id = NULL), s_null))),
   "2.4 no policies anywhere: the save goes on (nothing to lose)")
v2 <- new.env(); eval(parse(viejo("R/pasivos_wizard_module.R"), encoding = "UTF-8"), v2)
w0 <- wz(c("p1", "p2"))
ok(isTRUE(isolate(v2$.wiz_politicas_autoriza(w0, s_null))) && !length(isolate(w0$draft$policy_ids)),
   "2.5 contraprueba: 331e6146 saved with no policies in silence")

cat("== 3 & 5. the refresh ==\n")
L <- renta("Renta C", "p_fest,p_hab", extra = list(documento_template = "R-{YYYY}{MM}{DD}"))
base <- pasivos_generate_provisions(renta("Renta C", NA_character_, extra = list(documento_template = "R-{YYYY}{MM}{DD}")),
                                   window_start = HOY, window_end = as.Date("2027-06-30"), client_id = "c", policies_for_liability = list())
base$liability_id <- L$id; base$estado <- "provisional"
ene <- which(as.Date(base$fecha_calculada) == as.Date("2027-01-01")); feb <- which(as.Date(base$fecha_calculada) == as.Date("2027-02-01"))
ctx <- list(catalogo = catalogo, overrides = NULL, pagadas = base$id[feb])     # Feb paid ahead, still "provisional"
rr <- pasivos_refresco_calcular(L, base, HOY, "c", ctx)
t <- rr$tabla
ok(identical(as.Date(t$fecha_efectiva[ene]), as.Date("2027-01-04")), "3.0 Jan is re-placed (1 Jan, holiday -> Mon 4)")
ok(identical(as.Date(t$fecha_efectiva[feb]), as.Date("2027-02-01")), "3.1 a provision with a live payment never moves, though still provisional")
ok(identical(t$documento[ene], "R-20270104"), "5.2 the re-placed one's document is built from its new date (as the generator builds it)")
v3 <- new.env(parent = globalenv()); eval(parse(viejo("R/pasivos_engine.R"), encoding = "UTF-8"), v3)
t0 <- v3$pasivos_refresco_calcular(L, base, HOY, "c", ctx)$tabla
ok(!identical(as.Date(t0$fecha_efectiva[feb]), as.Date("2027-02-01")) && !identical(t0$documento[ene], "R-20270104"),
   "3.2 / 5.3 contraprueba: 331e6146 moved the paid one and kept the old document")

cat("== 4. the years of the holidays ==\n")
largo <- renta("Largo", "p_fest"); largo$fecha_vence <- as.Date("2030-12-31")
fz <- attr(pasivos_politicas_de(largo, catalogo), "festivos")[["MX"]]
ok(as.Date("2029-01-01") %in% fz && as.Date("2030-12-25") %in% fz, "4.1 2029 and 2030 holidays are there for a liability that runs to 2030")
fz0 <- attr(v3$pasivos_politicas_de(largo, catalogo), "festivos")[["MX"]]
ok(!as.Date("2029-01-01") %in% fz0, "4.2 contraprueba: 331e6146 had only the server's years")

cat("== 5. never in the past ==\n")
M <- renta("Menos cinco", "p_menos5", dia = 28L, extra = list(fecha_inicio = as.Date("2026-11-28")))
g <- pasivos_generate_provisions(M, window_start = HOY, window_end = as.Date("2027-01-31"), client_id = "c",
                                 policies_for_liability = pasivos_politicas_de(M, catalogo), today = HOY)
nov <- as.Date(g$fecha_efectiva[as.Date(g$fecha_calculada) == as.Date("2026-11-28")])
ok(identical(nov, HOY), "5.1 28 Nov minus 5 days would be 23 Nov (past): it is today, the next valid day")
g0 <- v3$pasivos_generate_provisions(M, window_start = HOY, window_end = as.Date("2027-01-31"), client_id = "c",
                                     policies_for_liability = v3$pasivos_politicas_de(M, catalogo), today = HOY)
ok(identical(as.Date(g0$fecha_efectiva[as.Date(g0$fecha_calculada) == as.Date("2026-11-28")]), as.Date("2026-11-23")),
   "5.4 contraprueba: 331e6146 put it on 23 Nov, in the past")

cat("== 6. «+ Nueva política» in the wizard, end to end (what Chrome could not see: writes were void there) ==\n")
# The in-memory store DOES write: the catalog saved is the one read back.
log_action <- function(...) invisible(NULL)
s3_memoria_reset()
assign(paste0(CID, "/", S3_KEYS$policy_catalog), catalogo, envir = .mem)
punto6 <- function() {
  r <- list()
  sh <- sesion(); sh$home_client_id <- function() CID
  sh$current_user_info <- reactive(list(username = "luis", tier = "admin"))
  testServer(function(input, output, session) setup_pasivos_wizard(input, output, session, sh), {
    session$flushReact()
    session$setInputs(pasivos_edit_liability = A$id)                      # A has p1
    html(output$pwiz_politicas_ui)
    session$setInputs(pwiz_pol_nueva = 1)                                 # the embedded editor
    r$editor <<- grepl('id="pwiz_pol_name"', html(output$pwiz_pol_editor_ui), fixed = TRUE)
    session$setInputs(pwiz_pol_name = "Nueva de prueba", pwiz_pol_type = "weekdays", pwiz_pol_param_days = c("1", "2", "3"))
    session$setInputs(pwiz_pol_btn_save = 1)
    cat_s3 <- get(paste0(CID, "/", S3_KEYS$policy_catalog), envir = .mem)
    r$nueva <<- cat_s3$id[cat_s3$name == "Nueva de prueba"]
    r$sel <<- seleccion(html(output$pwiz_politicas_ui))
    r$draft <<- sort(isolate(as.character(unlist(sh$policy_catalog_db()$id))))
    # the sync bus reads the catalog again from S3 (another session, or this one's own bump)
    sh$policy_catalog_db(load_policy_catalog(client_id = CID)); session$flushReact()
    r$sel_bus <<- seleccion(html(output$pwiz_politicas_ui))
    # a bus read that does NOT have it yet (S3 behind): the draft keeps it; the
    # selector shows what the catalog has until it arrives
    sh$policy_catalog_db(catalogo); session$flushReact()
    r$sel_atras <<- seleccion(html(output$pwiz_politicas_ui))
    session$setInputs(pwiz_politicas = r$sel_atras)   # the browser sends what the redrawn selector holds
    sh$policy_catalog_db(load_policy_catalog(client_id = CID)); session$flushReact()
    r$sel_vuelve <<- seleccion(html(output$pwiz_politicas_ui))
  })
  r
}
r6 <- punto6()
ok(isTRUE(r6$editor), "6.1 «+ Nueva política» draws Configuración's editor inside the wizard (pwiz_pol_*)")
ok(length(r6$nueva) == 1L, "6.2 the new policy is written to the catalog (S3)")
ok(identical(r6$sel, sort(c("p1", r6$nueva))), "6.3 it comes back SELECTED on the liability, next to the one it had")
ok(identical(r6$sel_bus, sort(c("p1", r6$nueva))), "6.4 the bus reading the catalog again after the creation does not lose it")
ok(identical(r6$sel_vuelve, sort(c("p1", r6$nueva))),
   "6.5 even a bus read from behind (catalog without it) does not erase it from the draft: it shows again once the catalog has it")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
