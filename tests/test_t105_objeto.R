# =============================================================================
# tests/test_t105_objeto.R
# T105: a liability's "Objeto" -- what it finances or pays ("3 camiones",
# "construcción"). Optional, one short line (PASIVOS_OBJETO_MAX).
#   A  the schema and pasivos_objeto_limpio(): trimmed, one line, cut to the
#      limit, empty = NA.
#   B  additive: a liability saved before the field loads with objeto NA, and
#      loading writes nothing (no migration).
#   C  draft <-> liability, for every category (regular, financiero,
#      tarjeta); NA reaches the form as "".
#   D  the wizard: the field in the step with the name (every category), with
#      the limit as maxlength; reading the step cleans it; Revisión shows it.
#   E  the real save (in-memory S3): the liability keeps it, and the log (T72,
#      pasivos_log_audit) has it before and after.
#   F  the Lista: a visible Objeto column (so the search, which reads every
#      shown cell, finds it), a filter, and the Excel carries it.
# Run from project root:  Rscript --vanilla tests/test_t105_objeto.R
# =============================================================================
cat("=== test_t105_objeto ===\n\n")
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")
`%||%` <- function(a, b) if (!is.null(a) && !all(is.na(a))) a else b
source("R/monedas.R")
source("R/zona.R")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_wizard_steps.R", "R/pasivos_edit_confirm_module.R",
            "R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_list_module.R"))
  suppressMessages(source(f))
source("tests/helpers/hd_numero.R")
has_capability    <- function(user, cap) TRUE
permiso_nombre    <- function(p) p
bump_sync_version <- function(...) invisible(NULL)
fmt_money         <- function(x) format(round(x, 2), nsmall = 2)
AUDIT <- list()
pasivos_log_audit <- function(...) { AUDIT[[length(AUDIT) + 1L]] <<- list(...); invisible(NULL) }
load_papelera  <- function(...) data.frame()
save_papelera  <- function(df, ...) invisible(TRUE)
pasivos_edit_confirm_open <- function(...) invisible(NULL)
pasivos_pedir_editar <- function(lid) invisible(NULL)
base_moneda <- function(...) "MXN"

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
CID <- "acme"; HOY <- Sys.Date()
MAX <- PASIVOS_OBJETO_MAX
borrador <- function(...) modifyList(list(
  categoria = "regular", nombre = "Renta bodega", empresa = "NG", parte = "Inmobiliaria",
  moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = 15L),
  fecha_inicio = HOY, amount_default = 100), list(...))
POR_CATEGORIA <- list(
  regular    = borrador(),
  financiero = borrador(categoria = "financiero", flavor = "credito_simple", principal_original = 1e6,
                        tasa_actual = 0.12, plazo_meses = 36L, dia_pago = 15L, frecuencia_pago = "mensual",
                        metodo_amortizacion = "francesa"),
  tarjeta    = borrador(categoria = "tarjeta", tarjeta_closing_day = 5L, tarjeta_due_day = 25L,
                        recurrence_type = "monthly_day"))

# ── A ────────────────────────────────────────────────────────────────────────
cat("-- A. schema and cleaning --\n")
ok("objeto" %in% names(.schema_pasivos_liability()) && is.character(.schema_pasivos_liability()$objeto),
   "A: the liability schema has objeto (character)")
ok(MAX == 60L, "A: the limit is 60 characters")
ok(identical(pasivos_objeto_limpio("  3 camiones "), "3 camiones"), "A: trimmed")
ok(all(is.na(c(pasivos_objeto_limpio(""), pasivos_objeto_limpio("   "), pasivos_objeto_limpio(NA),
               pasivos_objeto_limpio(NULL), pasivos_objeto_limpio(character(0))))), "A: empty, blank, NA, NULL: NA")
largo <- strrep("construcción ", 10)
ok(nchar(pasivos_objeto_limpio(largo)) == MAX && startsWith(largo, pasivos_objeto_limpio(largo)), "A: longer than the limit: cut to it")
ok(identical(pasivos_objeto_limpio("3 camiones\nde volteo\t"), "3 camiones de volteo"), "A: one line (line breaks and tabs become spaces)")
ok(identical(.pasivos_objeto_texto(NA), "") && identical(.pasivos_objeto_texto(" x "), "x"), "A: for a form field, NA is \"\"")

# ── B ────────────────────────────────────────────────────────────────────────
cat("-- B. additive: a liability from before the field --\n")
viejo <- pasivos_wizard_draft_to_liability(borrador(), mode = "create", user = "ana"); viejo$id <- "L0"
viejo$objeto <- NULL
s3_memoria_reset()
.s3_write(viejo, S3_KEYS$pasivos_liabilities, client_id = CID)
clave <- paste0(CID, "/", S3_KEYS$pasivos_liabilities)
crudo_antes <- get(clave, envir = .mem)
cargado <- load_pasivos_liabilities(client_id = CID)
ok(!"objeto" %in% names(crudo_antes), "B: (the stored row has no objeto column)")
ok("objeto" %in% names(cargado) && nrow(cargado) == 1L && is.na(cargado$objeto), "B: it loads with objeto NA")
ok(identical(get(clave, envir = .mem), crudo_antes), "B: loading wrote nothing (no migration)")
ok(identical(pasivos_liability_to_draft(cargado)$objeto, ""), "B: its form field is empty")
vacio_o_na <- function(v) length(v) == 1L && (is.na(v) || identical(v, ""))
ok(identical(nrow(pasivos_lista_filas(cargado, NULL, NULL, "active", function(e) "")), 1L) &&
     vacio_o_na(pasivos_lista_filas(cargado, NULL, NULL, "active", function(e) "")$objeto),
   "B: the Lista shows it with an empty Objeto")
sin_col <- crudo_antes
ok(vacio_o_na(pasivos_lista_filas(sin_col, NULL, NULL, "active", function(e) "")$objeto), "B: the Lista also takes a table without the column")

# ── C ────────────────────────────────────────────────────────────────────────
cat("-- C. draft <-> liability, every category --\n")
for (cat_ in names(POR_CATEGORIA)) {
  l <- pasivos_wizard_draft_to_liability(modifyList(POR_CATEGORIA[[cat_]], list(objeto = " 3 camiones ")), mode = "create", user = "ana")
  ok(identical(l$objeto, "3 camiones"), sprintf("C: %s: saved, trimmed", cat_))
  ok(identical(pasivos_liability_to_draft(l)$objeto, "3 camiones"), sprintf("C: %s: back to the form", cat_))
  l2 <- pasivos_wizard_draft_to_liability(modifyList(POR_CATEGORIA[[cat_]], list(objeto = "")), mode = "create", user = "ana")
  ok(is.na(l2$objeto), sprintf("C: %s: empty saves NA", cat_))
}
ok(nchar(pasivos_wizard_draft_to_liability(borrador(objeto = largo), mode = "create", user = "ana")$objeto) == MAX,
   "C: a value longer than the limit is cut when saving (never stored longer)")
ok(is.na(pasivos_wizard_draft_to_liability(borrador(), mode = "create", user = "ana")$objeto), "C: a draft without objeto saves NA")

# ── D ────────────────────────────────────────────────────────────────────────
cat("-- D. the wizard --\n")
for (cat_ in names(POR_CATEGORIA)) {
  h <- paste(as.character(pasivos_step_ui_detalles(modifyList(POR_CATEGORIA[[cat_]], list(objeto = "construcción")),
                                                    empresa_choices = c(NG = "NG"))), collapse = "")
  campo <- regmatches(h, regexpr('<input[^>]*id="pwiz_objeto"[^>]*>', h))
  ok(length(campo) == 1L && grepl(sprintf('maxlength="%d"', MAX), campo) && grepl('value="construcción"', campo, fixed = TRUE),
     sprintf("D: %s: the step with the name has Objeto, with maxlength %d and its value", cat_, MAX))
  ok(grepl(tr("tablas.pasivos.objeto.etiqueta"), h, fixed = TRUE) &&
       grepl(tr("tablas.pasivos.objeto.ayuda", max = MAX), h, fixed = TRUE), sprintf("D: %s: its label and help from the dictionary", cat_))
}
d <- .wiz_collect_step(list(pwiz_nombre = "Camiones", pwiz_objeto = "  3 camiones\n"), "detalles",
                       list(draft = list(categoria = "regular")))
ok(identical(d$objeto, "3 camiones"), "D: reading the step cleans the field")
d0 <- .wiz_collect_step(list(pwiz_nombre = "Camiones"), "detalles", list(draft = list(categoria = "regular")))
ok(identical(d0$objeto, ""), "D: a field never sent reads as empty")
rv <- paste(as.character(.wiz_revision_summary_html(borrador(objeto = "construcción"))), collapse = "")
ok(grepl(tr("tablas.pasivos.objeto.resumen"), rv, fixed = TRUE) && grepl("construcción", rv, fixed = TRUE), "D: Revisión shows it")
rv0 <- paste(as.character(.wiz_revision_summary_html(borrador())), collapse = "")
ok(!grepl(tr("tablas.pasivos.objeto.resumen"), rv0, fixed = TRUE), "D: Revisión says nothing when it is empty")
for (idioma in c("es", "en")) {
  dic <- jsonlite::fromJSON(sprintf("www/i18n/%s.json", idioma))
  ok(all(paste0("tablas.pasivos.objeto.", c("etiqueta", "marcador", "ayuda", "resumen", "columna")) %in% names(dic)),
     sprintf("D: the five keys in %s.json", idioma))
}

# ── E ────────────────────────────────────────────────────────────────────────
cat("-- E. the real save and its log --\n")
liab0 <- pasivos_wizard_draft_to_liability(borrador(), mode = "create", user = "ana"); liab0$id <- "L1"
provs <- pasivos_generate_provisions(liab0, window_start = HOY)
s3_memoria_reset()
save_pasivos_liabilities(liab0, client_id = CID)
.s3_write(provs, S3_KEYS$pasivos_provisions, client_id = CID)
L <- reactiveVal(liab0); P <- reactiveVal(provs)
sh <- list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
           company_map = function() list(NG = "Nuevo Grupo"), holiday_overrides_db = function() list(),
           effective_client_id = function() CID, current_user = function() "ana",
           suppress_ledger_prov_refresh = function(...) invisible(NULL))
wiz <- list(mode = "edit", edit_target_id = "L1", draft = modifyList(pasivos_liability_to_draft(liab0), list(objeto = "3 camiones")))
AUDIT <- list(); res <- NULL
testServer(function(input, output, session) res <<- .pasivos_wizard_save(wiz, sh, session, "ana"), {})
s3l <- load_pasivos_liabilities(client_id = CID)
ok(isTRUE(res$ok) && identical(s3l$objeto[s3l$id == "L1"], "3 camiones"), "E: editing saves the Objeto on S3")
ed <- Filter(function(x) identical(x$action_type, "liability.edited"), AUDIT)
ok(length(ed) == 1L && is.na(ed[[1]]$before$objeto) && identical(ed[[1]]$after$objeto, "3 camiones"),
   "E: the log has it before (empty) and after")
s3p <- load_pasivos_provisions(client_id = CID)
ok(nrow(s3p) == nrow(provs) && !"objeto" %in% names(s3p), "E: the provisions are untouched (the Objeto is the liability's)")

# ── F ────────────────────────────────────────────────────────────────────────
cat("-- F. the Lista --\n")
CAPT <- list()
.hd_table_server_real <- hd_table_server
hd_table_server <- function(id, ...) { CAPT[[id]] <<- list(...); .hd_table_server_real(id, ...) }
otro <- pasivos_wizard_draft_to_liability(borrador(nombre = "Obra", objeto = "construcción"), mode = "create", user = "ana"); otro$id <- "L2"
liabs <- dplyr::bind_rows(s3l, otro)
sh2 <- list(pasivos_liabilities_db = reactiveVal(liabs), pasivos_provisions_db = reactiveVal(provs),
            visible_initials = reactiveVal(NULL), effective_client_id = function() CID,
            current_user = function() "ana", suppress_ledger_prov_refresh = function(...) invisible(NULL))
datos <- NULL
testServer(pasivos_list_module_server, args = list(shared = sh2), {
  session$flushReact()
  datos <<- lista_datos()
})
hd_table_server <- .hd_table_server_real
ok(identical(datos$objeto[match(c("L1", "L2"), datos$id)], c("3 camiones", "construcción")), "F: each row carries its Objeto")
cols <- CAPT$lista$columnas
co <- Find(function(c) identical(c$campo, "objeto"), cols)
ok(!is.null(co) && identical(co$tipo, "texto") && isTRUE(co$visible %||% TRUE) && identical(co$titulo, "tablas.pasivos.objeto.columna"),
   "F: an Objeto column, shown (the search reads every shown cell)")
ok(isTRUE(co$recorte) && is.numeric(co$ancho) && co$ancho <= 160, "F: and small (fixed width, cut, whole in its bubble)")
ok(which(vapply(cols, `[[`, "", "campo") == "objeto") == which(vapply(cols, `[[`, "", "campo") == "nombre") + 1L,
   "F: right after Nombre")
ok(any(vapply(CAPT$lista$filtros, function(f) identical(f$campo, "objeto"), logical(1))), "F: a filter by Objeto")
arch <- tempfile(fileext = ".xlsx")
hd_excel_escribir(datos, cols, datos$id, "id", arch)
x <- openxlsx::read.xlsx(arch, check.names = FALSE)
ox <- x[[tr("tablas.pasivos.objeto.columna")]]
ok(tr("tablas.pasivos.objeto.columna") %in% names(x) && setequal(ox[!is.na(ox) & nzchar(ox)], c("3 camiones", "construcción")),
   "F: the Excel has the Objeto column with its values")
unlink(arch)
ok(is.null(Find(function(c) identical(c$campo, "objeto"), CAPT$papelera$columnas %||% list())), "F: (the Papelera is unchanged)")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
