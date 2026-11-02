# =============================================================================
# tests/helpers/empresas_fixture_app.R
# The Empresas tab (R/empresas_module.R) over fixed companies and a simulated
# `shared` -- no S3. save_empresas() keeps the table in memory (and fails on
# purpose with EMP_FALLA = 1), and every save, rename and log_action() call is
# listed in #registro as one line:
#   guardar <id-prefix>:<initials>:<razon_social>:<activa>:<deleted> | ...
#   renombrar OLD->NEW        log <action> <target_id>
# Buttons outside the module play "another session": #otra_sesion puts a new
# company FIRST in shared$empresas_db (every position shifts by one), and
# #otra_sesion_borra drops the first company, as the sync bus would.
#
# EMP_RAIZ: the checkout whose R/ and www/ to use (default: this one).
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("EMP_RAIZ", "."), winslash = "/")
assign("%||%", function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x,
       envir = globalenv())
cargar <- function(archivo, nombres) {
  ex <- parse(file.path(raiz, archivo), keep.source = FALSE)
  for (e in ex)
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
suppressPackageStartupMessages({ library(dplyr); library(shinyjs) })
cargar("R/persistence.R", c(".schema_empresas", ".schema_erp_connections", ".normalize", ".company_map_from_empresas"))
cargar("R/erp_connector_registry.R", c("SAP_B1_ERP_TYPE", "erp_connector_label"))
cargar("R/ui_components.R", "app_styles")
local({
  owd <- setwd(raiz); on.exit(setwd(owd))
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/empresas_module.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
})
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())

# ── What the module reaches outside itself, recorded ────────────────────────
registro  <- reactiveVal(character(0))
anotar    <- function(x) registro(c(registro(), x))
falla     <- identical(Sys.getenv("EMP_FALLA"), "1")
almacen   <- new.env()
assign("puede", function(usuario, permiso, ...) TRUE, envir = globalenv())
assign("bump_sync_version", function(...) invisible(NULL), envir = globalenv())
assign("erp_access_allowed", function(...) TRUE, envir = globalenv())
assign("sap_creds_source", function(...) NULL, envir = globalenv())
assign("load_erp_connections", function(...) .schema_erp_connections(), envir = globalenv())
assign(".s3_read_with", function(...) NULL, envir = globalenv())
assign("S3_KEYS", list(erp_connections = "erp_connections.rds"), envir = globalenv())
assign("save_empresas", function(df, client_id = NULL) {
  if (falla) stop("S3 no disponible (fixture)")
  almacen$df <- df
  anotar(paste("guardar", paste(sprintf("%s:%s:%s:%s:%s", substr(df$id, 1, 4), df$initials, df$razon_social,
                                        df$activa, df$deleted), collapse = " | ")))
  invisible(TRUE)
}, envir = globalenv())
assign("rename_empresa_initials", function(old, new, client_id = NULL) anotar(sprintf("renombrar %s->%s", old, new)),
       envir = globalenv())
assign("log_action", function(user, module, action, description = "", target_id = NA, ...)
  anotar(sprintf("log %s %s", action, target_id %||% "")), envir = globalenv())

emp <- function(id, code, ini, razon, rfc, activa = TRUE, deleted = FALSE)
  data.frame(id = id, account_code = code, initials = ini, razon_social = razon, nombre_corto = razon,
             nombres_alt = "[]", rfc = rfc, activa = activa, deleted = deleted, deleted_at = NA_character_,
             created_at = "2026-01-01", updated_at = "2026-01-01", stringsAsFactors = FALSE)
empresas0 <- .normalize(rbind(
  emp("e-ng",  "E0001", "NG",  "Networks Group LCT", "NGX000101AA1"),
  emp("e-nts", "E0002", "NTS", "Networks Trucking Services S.A. de C.V.", "NTX000101BB2"),
  emp("e-ncs", "E0003", "NCS", "Networks Crossdocking Services S.A. de C.V.", "NCX000101CC3"),
  emp("e-old", "E0004", "OLD", "Empresa Borrada SA", "OLD000000XX0", activa = FALSE, deleted = TRUE),
  emp("e-tst", "E0005", "TEST", "TEST EMPRESA SA", "TEST6715RFC01", activa = FALSE)
), .schema_empresas)

ui <- bslib::page_fluid(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  shinyjs::useShinyjs(), app_styles(),
  div(class = "d-flex gap-2 p-2",
      actionButton("otra_sesion", "Otra sesión: agrega una empresa al principio"),
      actionButton("otra_sesion_borra", "Otra sesión: quita la primera")),
  empresasUI("emp"),
  tags$pre(id = "registro", style = "font-size:.7rem; max-height:200px; overflow:auto;"),
  tags$style("#registro:empty { display: none; }"),
  tags$script(HTML("Shiny.addCustomMessageHandler('registro', function (t) { document.getElementById('registro').textContent = t; });"))
)

server <- function(input, output, session) {
  empresas_db <- reactiveVal(empresas0)
  shared <- list(
    empresas_db         = empresas_db,
    company_map         = reactive(.company_map_from_empresas(empresas_db())),
    effective_client_id = function() "fixture",
    home_client_id      = function() "fixture",
    current_user        = function() "luis",
    usuario             = function() "luis",
    current_user_info   = reactive(list(tier = "dev", is_staff = FALSE)),
    jump_client_id      = function() NULL,
    visible_initials    = function() NULL,
    group_config        = function() NULL
  )
  empresasServer("emp", shared)
  # "Another session" changes the companies underneath this one, through
  # shared$empresas_db as the sync bus does.
  observeEvent(input$otra_sesion, {
    nueva <- .normalize(emp("e-new", "E0009", "AAA", "Alfa Nueva SA", "AAA000000AA0"), .schema_empresas)
    empresas_db(rbind(nueva, empresas_db()))
  })
  observeEvent(input$otra_sesion_borra, empresas_db(empresas_db()[-1, , drop = FALSE]))
  observe(session$sendCustomMessage("registro", paste(registro(), collapse = "\n")))
}

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
