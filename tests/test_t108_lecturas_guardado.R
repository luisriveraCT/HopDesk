# =============================================================================
# tests/test_t108_lecturas_guardado.R
# T108 (4): the wizard's save reads what it needs once, never the strict read
# of the tables it writes less than once.
#   R  one save of the wizard: one strict read each of liabilities, provisions,
#      empresas (the CM29 map, shared by both writes) and the sync stamp; one
#      write of the stamp, with both tables' versions up
#   C  the map is never kept between saves: a rename between two saves is
#      applied in the second (each save reads empresas once)
#   F  provisions fail AFTER liabilities were written: the liabilities are
#      still announced (their bump alone), the provisions are not
#   V  bump_sync_version(c(a, b)): S3's stamp merged per name (the larger
#      number), both up by one, one write; a stamp that cannot be read is not
#      written (both up in memory, FALSE); one name as before
# Real wizard save, real sync bus, in-memory S3 (tests/helpers/s3_memoria.R).
# Run from project root:  Rscript tests/test_t108_lecturas_guardado.R
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble); library(lubridate); library(uuid) })
raiz <- normalizePath(".", winslash = "/")
source("tests/helpers/s3_memoria.R")
cargar <- function(archivo, nombres) {
  ex <- parse(file.path(raiz, archivo), keep.source = FALSE)
  for (e in ex)
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
cargar("R/global.R", c("fmt_money", "%||%"))
cargar("R/data_pipeline.R", c("etiquetas_por_factura", ".etiquetas_doc_unica", ".politica_doc", ".politica_doc_unica",
                              "parse_sap_date", "ensure_dates", "ensure_documento", "ensure_moneda",
                              "standardize_party", "guess_doc_col", "guess_col", "clean_str"))
sys.source("R/zona.R", envir = globalenv())
sys.source("R/monedas.R", envir = globalenv())
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/pasivos_schemas.R", "R/bancos_persistence.R", "R/pasivos_persistence.R",
            "R/pasivos_capabilities.R", "R/pasivos_audit.R", "R/forecasting_resolve.R",
            "R/policy_engine.R", "R/pasivos_engine.R", "R/pasivos_wizard_validate.R",
            "R/pasivos_wizard_module.R", "R/pasivos_edit_confirm_module.R", "R/sync_bus.R"))
  suppressMessages(sys.source(f, envir = globalenv()))
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())
assign("avisos_actividad",  function(...) invisible(TRUE), envir = globalenv())
assign("pasivos_log_audit", function(...) invisible(NULL), envir = globalenv())
assign("has_capability", function(user, cap) TRUE, envir = globalenv())
assign("pasivos_autoriza", function(...) TRUE, envir = globalenv())
assign("pasivos_edit_confirm_open", function(...) invisible(NULL), envir = globalenv())

.pass <- 0L; .fail <- 0L
ok <- function(cond, label) {
  if (isTRUE(cond)) { cat("  PASS ", label, "\n"); .pass <<- .pass + 1L }
  else              { cat("  FAIL ", label, "\n"); .fail <<- .fail + 1L }
}
quieto <- function(expr) suppressMessages(invisible(utils::capture.output(force(expr))))

# Every strict read and every write, by key.
LOG <- new.env()
leer_orig <- .s3_leer_para_escribir; escribir_orig <- .s3_write
.s3_leer_para_escribir <- function(key, ...) { LOG$lee <- c(LOG$lee, basename(key)); leer_orig(key, ...) }
.s3_write <- function(obj, key, ...) { LOG$escribe <- c(LOG$escribe, basename(key)); escribir_orig(obj, key, ...) }
cuenta <- function(v, k) sum(v == k)

CID <- "acme"; hoy <- Sys.Date()
reg <- function(id, monto, dia) {
  l <- pasivos_wizard_draft_to_liability(list(
    categoria = "regular", nombre = paste("Pasivo", id), empresa = "NG", parte = paste("Parte", id),
    moneda_pago = "MXN", recurrence_type = "monthly_day", recurrence_params = list(day = dia),
    fecha_inicio = hoy - 60, amount_default = monto), mode = "create", user = "ana")
  l$id <- id; l
}
liabs0 <- dplyr::bind_rows(reg("L1", 1000, 5L), reg("L2", 2000, 10L))
provs0 <- dplyr::bind_rows(lapply(seq_len(nrow(liabs0)), function(i) pasivos_generate_provisions(liabs0[i, ], window_start = hoy - 60)))
empresas <- function(iniciales = "NG", antes = NA_character_)
  tibble::tibble(id = "e1", initials = iniciales, nombre_corto = "Nuevo Grupo", iniciales_anteriores = antes)
sembrar <- function() {
  s3_memoria_reset()
  .SYNC_REGISTRY <<- list(); .GlobalEnv$.sync_versions <- list()
  register_synced("pasivos_provisions_db",  S3_KEYS$pasivos_provisions,  function(client_id = NULL) load_pasivos_provisions(client_id = client_id %||% CID))
  register_synced("pasivos_liabilities_db", S3_KEYS$pasivos_liabilities, function(client_id = NULL) load_pasivos_liabilities(client_id = client_id %||% CID))
  quieto(escribir_orig(empresas(), S3_KEYS$empresas, client_id = CID))
  quieto(save_pasivos_liabilities(liabs0, client_id = CID))
  quieto(escribir_orig(provs0, S3_KEYS$pasivos_provisions, client_id = CID))
}
nuevo_shared <- function() {
  L <- reactiveVal(load_pasivos_liabilities(client_id = CID)); P <- reactiveVal(load_pasivos_provisions(client_id = CID))
  list(pasivos_liabilities_db = L, pasivos_provisions_db = P,
       company_map = function() list(NG = "Nuevo Grupo", NG2 = "Nuevo Grupo"), holiday_overrides_db = function() list(),
       effective_client_id = function() CID, current_user = function() "ana",
       suppress_ledger_prov_refresh = function(...) invisible(NULL))
}
guardar <- function(lid, monto) {
  LOG$lee <- character(0); LOG$escribe <- character(0); res <- NULL
  testServer(function(input, output, session) {
    sh <- nuevo_shared()
    d <- pasivos_liability_to_draft(isolate(sh$pasivos_liabilities_db())[isolate(sh$pasivos_liabilities_db())$id == lid, ])
    d$amount_default <- monto
    quieto(res <<- .pasivos_wizard_save(list(mode = "edit", edit_target_id = lid, draft = d), sh, session, "ana"))
  }, {})
  res
}
ver <- function() unlist(.GlobalEnv$.sync_versions[c("pasivos_liabilities_db", "pasivos_provisions_db")])

# ── R ─────────────────────────────────────────────────────────────────────────
sembrar()
r <- guardar("L1", 1500)
ok(isTRUE(r$ok), "R el guardado sale bien")
ok(cuenta(LOG$lee, "pasivos_liabilities.rds") == 1L && cuenta(LOG$lee, "pasivos_provisions.rds") == 1L,
   "R una lectura estricta de cada tabla que se escribe (regla 17, CM09)")
ok(cuenta(LOG$lee, "empresas.rds") == 1L, sprintf("R empresas se lee una vez para las dos escrituras (lecturas: %d)", cuenta(LOG$lee, "empresas.rds")))
ok(cuenta(LOG$lee, "sync_versions.rds") == 1L && cuenta(LOG$escribe, "sync_versions.rds") == 1L,
   sprintf("R el sello se lee y se escribe una vez (lecturas %d, escrituras %d)", cuenta(LOG$lee, "sync_versions.rds"), cuenta(LOG$escribe, "sync_versions.rds")))
ok(identical(unname(ver()), c(1L, 1L)), "R y sube las dos versiones")

# ── C ─────────────────────────────────────────────────────────────────────────
sembrar()
quieto(guardar("L1", 1500))
quieto(escribir_orig(empresas("NG2", '["NG"]'), S3_KEYS$empresas, client_id = CID))   # a rename between the saves
quieto(guardar("L2", 2500))
ok(cuenta(LOG$lee, "empresas.rds") == 1L, "C el segundo guardado lee empresas otra vez (nada guardado entre guardados)")
ok(identical(unique(load_pasivos_liabilities(client_id = CID)$empresa), "NG2"),
   "C el renombre de entre guardados se aplica a los pasivos en el segundo")
ok(identical(unique(load_pasivos_provisions(client_id = CID)$empresa[load_pasivos_provisions(client_id = CID)$liability_id == "L2"]), "NG2"),
   "C y a las provisiones del pasivo guardado")

# ── F ─────────────────────────────────────────────────────────────────────────
sembrar()
guardar_provs_real <- save_pasivos_provisions
save_pasivos_provisions <- function(...) stop("simulated S3 write failure")
r <- guardar("L1", 1500)
save_pasivos_provisions <- guardar_provs_real
ok(isTRUE(load_pasivos_liabilities(client_id = CID)$amount_default[load_pasivos_liabilities(client_id = CID)$id == "L1"] == 1500),
   "F los pasivos se escribieron")
ok(identical(unname(ver()), c(1L, 0L)), sprintf("F fallaron las provisiones: se anuncian los pasivos solos (versiones %s)", paste(ver(), collapse = "/")))

# ── V ─────────────────────────────────────────────────────────────────────────
sembrar()
quieto(escribir_orig(list(pasivos_liabilities_db = 7L, pasivos_provisions_db = 2L, otra = 4L), S3_KEYS$sync_versions, client_id = CID))
LOG$escribe <- character(0)
okb <- bump_sync_version(c("pasivos_liabilities_db", "pasivos_provisions_db"), client_id = CID)
s <- .s3_leer_para_escribir(S3_KEYS$sync_versions, CID)
ok(isTRUE(okb) && identical(unname(ver()), c(8L, 3L)) && identical(s$otra, 4L) && cuenta(LOG$escribe, "sync_versions.rds") == 1L,
   "V un vector: el sello de S3 se fusiona por nombre (el mayor), los dos suben uno, una escritura")
okb <- bump_sync_version("pasivos_provisions_db", client_id = CID)
ok(isTRUE(okb) && identical(unname(ver()), c(8L, 4L)), "V un solo nombre, como siempre")
.mem_falla <- .s3_key(S3_KEYS$sync_versions, client_id = CID); LOG$escribe <- character(0)
okb <- suppressMessages(bump_sync_version(c("pasivos_liabilities_db", "pasivos_provisions_db"), client_id = CID))
.mem_falla <- character()
ok(identical(okb, FALSE) && identical(unname(ver()), c(9L, 5L)) && !cuenta(LOG$escribe, "sync_versions.rds"),
   "V un sello que no se puede leer no se escribe: suben en memoria y devuelve FALSE")
w <- NULL; okb <- withCallingHandlers(bump_sync_version(c("no_registrada", "pasivos_provisions_db"), client_id = CID),
                                      warning = function(c) { w <<- conditionMessage(c); invokeRestart("muffleWarning") })
ok(isTRUE(okb) && grepl("no_registrada", w %||% "") && identical(unname(ver()), c(9L, 6L)),
   "V un nombre no registrado avisa y se deja fuera; el resto sube")

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
