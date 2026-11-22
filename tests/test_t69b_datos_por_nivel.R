# =============================================================================
# tests/test_t69b_datos_por_nivel.R
# T69b: a tier with a tab list (Compras: PAN + REQ) reads the treasury
# reactives as their empty start value, whoever reads them -- not only
# hd_table: calendar cells, balances, charts, modals, Panorama's ribbon.
#   1. Every synced reactive (register_synced), plus sap_data,
#      sap_snapshot_info and bancos_cuentas_db, is classified: treasury
#      (HD_REACTIVOS_TESORERIA) or common (HD_REACTIVOS_COMUNES). And every
#      treasury name is a reactiveVal of app.R, so the wrap is never skipped.
#   2. nivel_reactivo_permitido(): Compras no / finance and staff yes /
#      unknown tier no (fail closed); common data always.
#   3. app.R's own .rv_por_nivel(), run: Compras reads the start value; the
#      same session back on a full tier sees the data again at once (no
#      reload, the reactive re-runs); writes go through under any tier;
#      crudo() always gives the stored value.
#   4. app.R: the wrap comes before `shared` is built; the process caches and
#      the SAP change checks read crudo().
#   5. Panorama, run for Compras with the gated (start) values: every section
#      draws with no error and no treasury amount; with full data it shows
#      them (the counter-proof that the check sees amounts).
# Run from project root:  Rscript tests/test_t69b_datos_por_nivel.R
# Counter-proof: on 39db7b8 (T69 without T69b) 1, 2, 3 and 4 fail.
# =============================================================================
cat("=== test_t69b_datos_por_nivel ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
`%||%` <- function(a, b) if (is.null(a)) b else a
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
.i18n_dir <- file.path(getwd(), "www", "i18n")
for (f in c("R/i18n.R", "R/monedas.R", "R/agenda_filas.R", "R/panorama_datos.R", "R/ui_components.R",
            "R/tier_registry.R", "R/permisos.R", "R/avisos_umbrales.R", "R/tema_registro.R", "R/avisos_correos.R"))
  source(f)
COMPRAS <- "compras"
TOTAL <- names(TIER_REGISTRY)[vapply(TIER_REGISTRY, function(e) is.null(e$pestanas) && !isTRUE(e$interno), logical(1))][1]
STAFF <- names(TIER_REGISTRY)[vapply(TIER_REGISTRY, function(e) isTRUE(e$interno), logical(1))][1]

app_src <- Sys.getenv("HD_APP_R", "app.R")
exprs <- parse(app_src, keep.source = FALSE, encoding = "UTF-8")
txt <- readLines(app_src, warn = FALSE, encoding = "UTF-8")
codigo <- sub("#.*$", "", txt)

# ── 1. Every synced reactive is classified ───────────────────────────────────
sincronizados <- character()
buscar_reg <- function(e) {
  if (!is.call(e)) return(invisible())
  if (identical(e[[1]], as.name("register_synced")) && length(e) >= 2 && is.character(e[[2]]))
    sincronizados <<- c(sincronizados, e[[2]])
  for (i in seq_along(e)[-1]) if (!identical(e[[i]], quote(expr = )) && is.call(e[[i]])) buscar_reg(e[[i]])
}
for (e in exprs) buscar_reg(e)
datos <- unique(c(sincronizados, "sap_data", "sap_snapshot_info", "bancos_cuentas_db"))
sin <- setdiff(datos, c(names(HD_REACTIVOS_TESORERIA), HD_REACTIVOS_COMUNES))
ok(sprintf("every synced reactive is classified (%d found; unclassified: %s)", length(sincronizados),
           if (length(sin)) paste(sin, collapse = ",") else "none"),
   length(sincronizados) > 20L && !length(sin))
rvs <- unique(sub("^\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\s*<-\\s*reactiveVal\\(.*$", "\\1",
                  grep("^\\s*[A-Za-z_.][A-Za-z0-9_.]*\\s*<-\\s*reactiveVal\\(", codigo, value = TRUE)))
falta <- setdiff(names(HD_REACTIVOS_TESORERIA), rvs)
ok(sprintf("every treasury name is a reactiveVal of app.R (missing: %s)", if (length(falta)) paste(falta, collapse = ",") else "none"),
   !length(falta))
ok("the two lists do not overlap", !length(intersect(names(HD_REACTIVOS_TESORERIA), HD_REACTIVOS_COMUNES)))
ok("every tab named in the map is a client tab",
   all(unlist(HD_REACTIVOS_TESORERIA) %in% c("CAL", "VEN", "PH", "BNC", "IC", "PSV", "FC", "RPT")))

# ── 2. Who may read what ─────────────────────────────────────────────────────
ok("Compras may read no treasury reactive",
   !any(vapply(names(HD_REACTIVOS_TESORERIA), function(n) nivel_reactivo_permitido(COMPRAS, n), logical(1))))
ok("Compras reads the common data (empresas, proveedores…)",
   all(vapply(HD_REACTIVOS_COMUNES, function(n) nivel_reactivo_permitido(COMPRAS, n), logical(1))))
ok("a full client tier and staff read everything",
   all(vapply(names(HD_REACTIVOS_TESORERIA), function(n) nivel_reactivo_permitido(TOTAL, n) && nivel_reactivo_permitido(STAFF, n), logical(1))))
ok("an unknown tier reads no treasury reactive (fail closed)",
   !any(vapply(list(NULL, "", NA_character_, "no_es_nivel"), function(t) nivel_reactivo_permitido(t, "pagar_hoy_db"), logical(1))))

# ── 3. app.R's own wrapper, run ──────────────────────────────────────────────
def <- list()
buscar_def <- function(e) {
  if (!is.call(e)) return(invisible())
  if (identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% c(".rv_por_nivel", "crudo"))
    def[[as.character(e[[2]])]] <<- e
  for (i in seq_along(e)[-1]) if (!identical(e[[i]], quote(expr = )) && is.call(e[[i]])) buscar_def(e[[i]])
}
for (e in exprs) buscar_def(e)
ok("app.R defines .rv_por_nivel() and crudo()", all(c(".rv_por_nivel", "crudo") %in% names(def)))
if (all(c(".rv_por_nivel", "crudo") %in% names(def))) {
  testServer(function(input, output, session) {
    ent <- environment()
    nivel <- reactiveVal(COMPRAS)
    assign("tier_efectivo", function() nivel(), envir = ent)
    eval(def[[".rv_por_nivel"]], ent); eval(def[["crudo"]], ent)
    datos_rv <- reactiveVal(data.frame(x = integer()))
    ph <- .rv_por_nivel(datos_rv, "pagar_hoy_db")
    comun <- .rv_por_nivel(reactiveVal("empresas"), "empresas_db")
    lectura <- reactive(ph())
    session$userData$t <- list(nivel = nivel, ph = ph, crudo = crudo, lectura = lectura, comun = comun)
  }, {
    t <- session$userData$t
    t$ph(data.frame(x = 1:3))                 # a load writes, whatever the tier
    session$flushReact()
    ok("Compras reads the start value (no rows)", nrow(t$lectura()) == 0L)
    guardado <- t$crudo(t$ph)   # (two steps: test_aislar_global_guardian's parser reads f(x)() badly)
    ok("the write went through: crudo() has the 3 rows", nrow(guardado()) == 3L)
    t$nivel(TOTAL); session$flushReact()
    ok("the same session back on a full tier sees the 3 rows at once (the reactive re-ran)", nrow(t$lectura()) == 3L)
    t$nivel(""); session$flushReact()
    ok("tier unknown again: the start value (fail closed)", nrow(t$lectura()) == 0L)
    t$nivel(COMPRAS)
    ok("a wrapped common name still reads its value for Compras", identical(isolate(t$comun()), "empresas"))
  })
}

# ── 4. app.R's wiring ────────────────────────────────────────────────────────
i_wrap <- grep("for (.nm in names(HD_REACTIVOS_TESORERIA))", codigo, fixed = TRUE)[1]
i_shared <- grep("^  shared <- list\\(", codigo)[1]
ok("app.R wraps the treasury reactives before building `shared`", !is.na(i_wrap) && !is.na(i_shared) && i_wrap < i_shared)
cache <- grep('.cache_set("', codigo, fixed = TRUE, value = TRUE)
tes_cache <- cache[!grepl('"proveedores"', cache, fixed = TRUE)]
ok(sprintf("the process caches read the stored value, crudo() (%d)", length(tes_cache)),
   length(tes_cache) >= 10L && all(grepl("crudo(", tes_cache, fixed = TRUE)))
ok("the SAP change checks read crudo(sap_data)",
   any(grepl("current    <- crudo(sap_data)()", codigo, fixed = TRUE)) &&
     any(grepl("cur <- crudo(sap_data)()[[ledger_name]]", codigo, fixed = TRUE)) &&
     any(grepl("!identical(crudo(sap_data)()$AR, cache$AR)", codigo, fixed = TRUE)))

# ── 5. Panorama for Compras, run with the start values ───────────────────────
for (f in c("R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R", "R/avisos.R",
            "R/avisos_alertas_module.R", "R/panorama_publicar_module.R", "R/panorama_module.R"))
  suppressMessages(source(f))
HOY <- Sys.Date()
ph_lleno <- data.frame(ledger = "AP", status = "pending", tipo_item = "factura", Moneda = "MXN",
                       Importe = c(123456, 200), FechaVenc = HOY, stringsAsFactors = FALSE)
ar_lleno <- data.frame(FechaEff = HOY, Moneda = "MXN", `Saldo vencido` = 654321, check.names = FALSE)
pintar <- function(tier, ph, provs, sapinfo, ar) {
  shared <- list(
    effective_client_id = function() "cli",
    usuario = function() list(user = "ana", tier = tier, permisos = list()),
    current_user_info = function() list(user = "ana", name = "Ana", is_staff = FALSE, tier = tier),
    tier_efectivo = function() tier, pagar_hoy_db = function() ph,
    pasivos_provisions_db = function() provs, sap_snapshot_info = function() sapinfo,
    df_combined_AR = function() ar, fx_fix = function() NULL, moneda_base_cliente = function() list(moneda = "MXN"))
  out <- NULL; err <- NULL
  tryCatch(testServer(panoramaServer, args = list(shared = shared, activa = shiny::reactive(FALSE)), {
    session$flushReact()
    out <<- lapply(c("encabezado", "clima", "hoy", "cinta", "destacados", "solicitudes", "feed", "equipo"),
                   function(n) tryCatch(as.character(output[[n]]$html %||% output[[n]]),
                                        error = function(e) if (inherits(e, "shiny.silent.error")) "" else paste("ERROR", conditionMessage(e))))
  }), error = function(e) err <<- conditionMessage(e))
  list(out = paste(unlist(out), collapse = " "), err = err)
}
compras <- pintar(COMPRAS, NULL, NULL, list(AR = NULL, AP = NULL), NULL)   # what the gated reactives give at start
ok(paste("Panorama for Compras (start values) draws with no error:", compras$err %||% "ok"),
   is.null(compras$err) && !grepl("ERROR", compras$out, fixed = TRUE))
ok("Panorama for Compras shows no treasury amount", !grepl("123,656|123,456|654,321", compras$out))
lleno <- pintar(TOTAL, ph_lleno, NULL, list(AR = NULL, AP = NULL), ar_lleno)
ok("counter-proof: with the data (a full tier), Panorama shows the amounts", grepl("123,656|654,321", lleno$out))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
