# =============================================================================
# tests/test_t111_iva_alta.R
# T111 (prod, Luis): adding a NEW financiero and typing IVA in Impuestos, the
# IVA is not saved; editing one that already has it works.
# The real wizard (setup_pasivos_wizard) in testServer, step by step: alta,
# categoría, detalles, términos, cargos (+ Agregar impuesto, IVA 16 % on the
# interest), Revisión, Guardar -- on in-memory S3.
# The browser, modelled: the tax rows are a nested uiOutput
# (pwiz_impuestos_table) inside the step's uiOutput. When the step is drawn
# again, Shiny's client paints that nested output from its CACHED last value
# (it never asks the server again) and its fields send what that HTML holds.
# That cached HTML is what the server rendered when the rows last changed --
# for a new liability, the empty row "+ Agregar impuesto" made. So:
#   A  straight through (no redraw): saved.
#   B  the step is drawn again while on Cargos (a validation message, or the
#      sync bus refreshing the liabilities), then Siguiente: the IVA must be
#      saved. Before the fix the typed row became the cached empty one and
#      was dropped silently.
#   C  back from Revisión to Cargos and forward again: the IVA must be saved.
#   D  editing a liability that already has IVA: kept (why "editar" worked).
# Run from project root:  Rscript --vanilla tests/test_t111_iva_alta.R
# =============================================================================
cat("=== test_t111_iva_alta ===\n\n")
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid)
})
source("tests/helpers/s3_memoria.R")
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
source("R/monedas.R"); source("R/zona.R")
for (f in c("R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R", "R/pasivos_wizard_steps.R",
            "R/pasivos_wizard_module.R", "R/pasivos_edit_confirm_module.R", "R/i18n.R", "R/pasivos_table_pivot.R",
            "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/ui_components.R"))
  suppressMessages(source(f))
for (e in parse("R/global.R", keep.source = FALSE))
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && identical(e[[2]], as.name("strip_accents"))) eval(e)
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
isTRUE_safe <- function(x) !is.na(x) & x
bump_sync_version <- function(...) invisible(NULL)
avisos_actividad <- function(...) invisible(TRUE)
pasivos_log_audit <- function(...) invisible(NULL)
pasivos_edit_confirm_open <- function(...) invisible(NULL)
has_capability <- function(...) TRUE
pasivos_autoriza <- function(...) TRUE
forecasting_get_estimate <- function(...) NA_real_

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
callado <- function(expr) invisible(utils::capture.output(suppressMessages(expr)))
CID <- "acme"

sesion <- function(liabs = .schema_pasivos_liability()) {
  L <- reactiveVal(liabs); P <- reactiveVal(.schema_pasivos_provision())
  list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
       effective_client_id = function() CID,
       suppress_ledger_prov_refresh = function(...) invisible(NULL),
       company_map = function() list(NG = "Nuevo Grupo"), holiday_overrides_db = function() list(),
       proveedores_db = function() NULL, moneda_base_cliente = function() list(moneda = "USD"),
       current_user = function() "luis")
}
html <- function(x) if (is.null(x)) "" else paste(as.character(if (is.list(x) && !is.null(x$html)) x$html else x), collapse = "")
# What the browser's fields hold after painting this HTML of the tax rows.
campos_de <- function(h) {
  v <- list()
  for (m in regmatches(h, gregexpr('<input[^>]*id="pwiz_imp_(nombre|valor)_[0-9]+"[^>]*>', h))[[1]]) {
    id <- sub('.*id="([^"]+)".*', "\\1", m)
    val <- if (grepl('value="', m)) sub('.*value="([^"]*)".*', "\\1", m) else ""
    v[[id]] <- if (grepl("valor", id)) suppressWarnings(as.numeric(val)) else val
  }
  for (m in regmatches(h, gregexpr('<select[^>]*id="pwiz_imp_(tipo|base)_[0-9]+".*?</select>', h))[[1]]) {
    id <- sub('.*id="([^"]+)".*', "\\1", substr(m, 1, 200))
    sel <- regmatches(m, regexpr('<option value="[^"]*" selected>', m))
    v[[id]] <- if (length(sel)) sub('<option value="([^"]*)".*', "\\1", sel) else NA_character_
  }
  v
}

# How many times the server drew each step (the browser repaints a step's
# nested rows from its cache only when the step is drawn again).
DIBUJOS <- list(cargos = 0L, terminos = 0L, detalles = 0L)
for (k in names(DIBUJOS)) local({
  paso <- k; real <- get(paste0("pasivos_step_ui_", paso))
  assign(paste0("pasivos_step_ui_", paso), function(...) { DIBUJOS[[paso]] <<- DIBUJOS[[paso]] + 1L; real(...) }, envir = globalenv())
})

# Drives the real wizard. `entre` runs while on Cargos after typing the IVA
# (the redraw), `atras_desde_revision` goes back from Revisión once.
recorrer <- function(sh, redibujar = FALSE, atras_desde_revision = FALSE, editar = NULL,
                     error_en_cargos = FALSE, atras_en_terminos = FALSE) {
  out <- list(paso_final = NA_character_, guardo = NA)
  testServer(function(input, output, session) setup_pasivos_wizard(input, output, session, sh), {
    session$flushReact()                         # the observers' first run (ignoreInit) before any click
    cache <- ""                                  # the client's cached tax rows
    paint <- function() {                        # the server sends new rows -> cache + fields
      h <- html(output$pwiz_impuestos_table)
      if (!identical(h, cache)) { cache <<- h; do.call(session$setInputs, campos_de(h)) }
    }
    redibujo_paso <- function(antes) {           # the step drawn again: fields from the cache
      html(output$pwiz_step_content)
      if (DIBUJOS$cargos > antes) do.call(session$setInputs, campos_de(cache))
    }
    siguiente <- function() { session$setInputs(pwiz_next = (input$pwiz_next %||% 0) + 1) }
    if (is.null(editar)) {
      session$setInputs(pasivos_add_liability = 1)
      session$setInputs(pwiz_categoria = "financiero", pwiz_flavor = "credito_simple"); siguiente()
      session$setInputs(pwiz_nombre = "Crédito camiones", pwiz_empresa = "NG", pwiz_parte = "Banco",
                        pwiz_moneda_pago = "USD", pwiz_cotizado_en = ""); siguiente()
      session$setInputs(pwiz_principal_original = 100000, pwiz_tasa_tipo = "fija", pwiz_tasa_anual = 8.75,
                        pwiz_fecha_inicio = as.Date("2026-12-25"), pwiz_plazo_meses = 14L, pwiz_frecuencia_pago = "mensual",
                        pwiz_dia_pago = 25L, pwiz_metodo_amortizacion = "francesa", pwiz_periodo_gracia = 0L)
      if (atras_en_terminos) {                     # Atrás to Detalles and Siguiente again, without Siguiente first
        session$setInputs(pwiz_back = 1); siguiente()
        h <- html(output$pwiz_step_content)
        out$plazo_tras_volver <<- regmatches(h, regexpr('id="pwiz_plazo_meses"[^>]*', h))
        # the browser: the fields as the step was drawn
        v <- regmatches(out$plazo_tras_volver, regexpr('(?<=value=")[^"]*', out$plazo_tras_volver, perl = TRUE))
        session$setInputs(pwiz_plazo_meses = suppressWarnings(as.integer(v)))
      }
      siguiente()
      html(output$pwiz_step_content)
      session$setInputs(pwiz_add_impuesto = 1); paint()
      session$setInputs(pwiz_imp_nombre_1 = "IVA", pwiz_imp_valor_1 = 16, pwiz_imp_tipo_1 = "porcentual",
                        pwiz_imp_base_1 = "intereses")
    } else {
      session$setInputs(pasivos_edit_liability = editar)
      session$setInputs(pwiz_goto_step = "cargos")
      html(output$pwiz_step_content); paint()
      out$editando <<- grepl("Editar", html(output$pwiz_modal_title), fixed = TRUE) &&
        grepl('value="IVA"', cache, fixed = TRUE)
      session$setInputs(pwiz_imp_valor_1 = 12)    # the person changes the rate
    }
    if (redibujar) {                               # the sync bus brings another session's liability
      antes <- DIBUJOS$cargos
      otro <- pasivos_wizard_draft_to_liability(list(categoria = "regular", nombre = "Otro", empresa = "NG", parte = "X",
        subcategoria = "renta_nueva", moneda_pago = "USD", recurrence_type = "monthly_day", recurrence_params = list(day = 1L),
        fecha_inicio = as.Date("2026-12-01"), amount_default = 1), mode = "create", user = "otra")
      sh$pasivos_liabilities_db(dplyr::bind_rows(isolate(sh$pasivos_liabilities_db()), otro))
      session$flushReact()                       # the bus's change reaches every reader
      redibujo_paso(antes); paint()
      out$redibujos_bus <<- DIBUJOS$cargos - antes
    }
    if (error_en_cargos) {                         # a message on Cargos (a negative residual), then fixed
      antes <- DIBUJOS$cargos
      session$setInputs(pwiz_has_residual = TRUE, pwiz_residual_monto = -5, pwiz_residual_moneda = "USD",
                        pwiz_residual_behavior = "replace_last")
      siguiente(); redibujo_paso(antes); paint()
      out$redibujos_error <<- DIBUJOS$cargos - antes
      session$setInputs(pwiz_has_residual = FALSE)
    }
    siguiente()                                   # Cargos -> Revisión
    if (atras_desde_revision) {
      antes <- DIBUJOS$cargos
      session$setInputs(pwiz_back = 1); redibujo_paso(antes); paint(); siguiente()
    }
    out$resumen <<- html(output$pwiz_revision_summary)
    session$setInputs(pwiz_save = 1)
  })
  l <- tryCatch(load_pasivos_liabilities(client_id = CID), error = function(e) NULL)
  out$liabs <- l
  out
}
iva_guardado <- function(l, nombre = NULL) {
  if (!is.data.frame(l) || !nrow(l)) return(FALSE)
  if (!is.null(nombre)) l <- l[l$nombre %in% nombre, , drop = FALSE]
  imps <- pasivos_impuestos_de(l[nrow(l), , drop = FALSE])
  length(imps) == 1L && identical(imps[[1]]$nombre, "IVA") && isTRUE(as.numeric(imps[[1]]$valor) == 16) &&
    identical(imps[[1]]$base, "intereses")
}

cat("-- A. straight through --\n")
s3_memoria_reset(); sh <- sesion()
callado(rA <- recorrer(sh))
ok(is.data.frame(rA$liabs) && nrow(rA$liabs) == 1L, "A: the new liability is saved")
ok(iva_guardado(rA$liabs), "A: with its IVA (16 %, on the interest)")

cat("-- B. the Cargos step drawn again while typing (sync bus) --\n")
s3_memoria_reset(); sh <- sesion()
callado(rB <- recorrer(sh, redibujar = TRUE))
ok(is.data.frame(rB$liabs) && nrow(rB$liabs) == 1L, "B: the new liability is saved")
ok(iva_guardado(rB$liabs), "B: THE BUG -- the IVA typed before the step was drawn again is saved")

cat("-- C. back from Revisión to Cargos and forward again --\n")
s3_memoria_reset(); sh <- sesion()
callado(rC <- recorrer(sh, atras_desde_revision = TRUE))
ok(iva_guardado(rC$liabs), "C: THE BUG -- the IVA survives going back to Cargos")

cat("-- E. a message on Cargos draws it again --
")
s3_memoria_reset(); sh <- sesion()
callado(rE <- recorrer(sh, error_en_cargos = TRUE))
ok(isTRUE(rE$redibujos_error >= 1L), "E: (the message did draw Cargos again)")
ok(iva_guardado(rE$liabs), "E: the IVA survives a validation message on Cargos")

cat("-- F. Términos: Atrás and Siguiente again, without Siguiente first --
")
s3_memoria_reset(); sh <- sesion()
callado(rF <- recorrer(sh, atras_en_terminos = TRUE))
ok(length(rF$plazo_tras_volver) == 1L && grepl('value="14"', rF$plazo_tras_volver, fixed = TRUE),
   "F: back on Términos, the plazo typed before Atrás is still there")
ok(iva_guardado(rF$liabs) && isTRUE(rF$liabs$plazo_meses[1] == 14L), "F: and the liability is saved with it")

cat("-- G. the sync bus never draws the step again --
")
ok(identical(rB$redibujos_bus, 0L), "G: another session's liability arriving does not draw Cargos again")

cat("-- D. editing one that already has IVA (why 'editar' worked) --\n")
base_l <- rA$liabs
s3_memoria_reset(); callado(save_pasivos_liabilities(base_l, client_id = CID)); sh <- sesion(base_l)
callado(rD <- recorrer(sh, redibujar = TRUE, editar = base_l$id[1]))
ok(isTRUE(rD$editando), "D: (really editing: the modal says Editar and Cargos shows the saved IVA)")
imp_d <- if (is.data.frame(rD$liabs) && nrow(rD$liabs)) pasivos_impuestos_de(rD$liabs[1, , drop = FALSE]) else list()
ok(length(imp_d) == 1L && identical(imp_d[[1]]$nombre, "IVA") && isTRUE(as.numeric(imp_d[[1]]$valor) == 12),
   "D: editing: the IVA is kept, with the rate typed (12) -- before, a redraw put back the saved 16")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
