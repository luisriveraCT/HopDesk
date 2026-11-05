# =============================================================================
# tests/helpers/requisiciones_fixture_app.R
# The Requisiciones tab (R/requisiciones_module.R) over an in-memory S3 local
# to this app: only the persistence helpers the Requisiciones files call are
# replaced (.s3_read_with, .s3_existe, .s3_write, .adj_s3_poner,
# .adj_s3_traer), in this process's global env; aws.s3 is never patched.
# The real write path (modificar_tabla on a strict fresh read) is the one
# from R/persistence.R.
#
#   REQ_RAIZ          checkout whose R/ and www/ to use (default: this one)
#   REQ_NIVEL         tier of the session's user (default "compras")
#   REQ_USUARIO       its username (default "dani")
#   HD_ARRANQUE_NULL  1 = the client id and the company map start NULL, as
#                     before the session knows them (incident 2026-11-03)
#   REQ_VACIO         1 = no seed data
#
# Buttons outside the module play "another session": #otra_sesion creates a
# requisition and #otra_sesion_paga confirms the payment of REQ-…-0002's
# provision, as other sessions would (in-memory counter bumped).
# =============================================================================
library(shiny)
raiz <- normalizePath(Sys.getenv("REQ_RAIZ", "."), winslash = "/")
assign("%||%", function(a, b) if (is.null(a)) b else a, envir = globalenv())
tomar <- function(archivo, nombres) {
  for (e in parse(file.path(raiz, archivo), keep.source = FALSE))
    if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres)
      eval(e, envir = globalenv())
}
suppressPackageStartupMessages({ library(dplyr); library(tibble) })
tomar("R/global.R", "S3_KEYS")
tomar("R/persistence.R", c(".normalize", "leer_tabla_para_escribir", ".s3_leer_para_escribir", "modificar_tabla",
                           "rebase_por_id", "confirmaciones_vigentes", ".schema_manual"))
tomar("R/bancos_persistence.R", ".schema_bancos_confirmados")
tomar("R/pasivos_persistence.R", c("etapa_de_provision_en", "PROVISION_PAGADA"))
local({
  owd <- setwd(raiz); on.exit(setwd(owd))
  for (f in c("R/pasivos_schemas.R", "R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/avisos.R",
              "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/adjuntos_s3.R", "R/requisiciones_persistence.R", "R/requisiciones_module.R"))
    sys.source(f, envir = globalenv())
})
assign(".i18n_dir", file.path(raiz, "www", "i18n"), envir = globalenv())

# ── In-memory S3 ─────────────────────────────────────────────────────────────
.req_mem  <- new.env(); .req_objs <- new.env()
assign(".s3_preload_cache", new.env(), envir = globalenv())
assign(".s3_missing_cache", new.env(), envir = globalenv())
assign(".req_falla_lectura", FALSE, envir = globalenv())   # set after the seed (REQ_FALLA_LECTURA)
assign(".s3_key", function(key, client_id = NULL) {
  if (is.null(client_id) || !nzchar(client_id)) stop("sin client_id en .s3_key")
  paste0(tolower(client_id), "/", key)
}, envir = globalenv())
assign(".s3_read_with", function(key, client_id = NULL) {
  if (isTRUE(.req_falla_lectura)) return(NULL)
  .req_mem[[.s3_key(key, client_id)]]
}, envir = globalenv())
assign(".s3_existe", function(key, client_id) {
  if (isTRUE(.req_falla_lectura)) return(NA)
  exists(.s3_key(key, client_id), envir = .req_mem, inherits = FALSE)
}, envir = globalenv())
assign(".s3_write", function(obj, key, client_id = NULL, permitir_reduccion = FALSE)
  assign(.s3_key(key, client_id), obj, envir = .req_mem), envir = globalenv())
assign(".adj_s3_poner", function(bytes, llave, mime) assign(llave, bytes, envir = .req_objs), envir = globalenv())
assign(".adj_s3_traer", function(llave) {
  if (!exists(llave, envir = .req_objs, inherits = FALSE)) stop("no existe ", llave)
  get(llave, envir = .req_objs)
}, envir = globalenv())
assign("log_action", function(...) invisible(NULL), envir = globalenv())
assign("auth_load_usuarios", function(client_id = NULL) NULL, envir = globalenv())

CID <- "fixture"
poner <- function(key, df) assign(paste0(CID, "/", key), df, envir = .req_mem)

# ── Seed ─────────────────────────────────────────────────────────────────────
sembrar <- function() {
  poner(S3_KEYS$pasivos_provisions, .normalize(tibble::tibble(
    id = c("p-luz", "p-renta", "p-libre1", "p-libre2", "p-pagada"),
    estado = c("provisional", "converted", "provisional", "provisional", "item_confirmed"),
    empresa = c("NCS", "NTS", "NCS", "NTS", "NCS"),
    parte = c("CFE", "Inmobiliaria Sur", "Telmex", "Grupo Constructor Norte del Bajío", "Papelería"),
    referencia = c("Luz bodega 3", "Renta noviembre", "Internet oficinas", "Anticipo obra nave 2", "Papelería oct"),
    amount_pago = c(18450.5, 92000, 4300, 250000, 3100), moneda_pago = c("MXN", "MXN", "MXN", "USD", "MXN"),
    fecha_efectiva = Sys.Date() + c(5, 12, 8, 20, -3)), .schema_pasivos_provision))
  poner(S3_KEYS$bancos_confirmados, .normalize(tibble::tibble(
    confirmacion_id = "c1", provision_id = "p-pagada", importe = 3100, moneda = "MXN",
    eliminado = FALSE, action = "confirmed", confirmado_at = Sys.time()), .schema_bancos_confirmados))
  c1 <- requisiciones_carpeta_crear(CID, list(nombre = "Obra nave 2", color = "#2e7d32", presupuesto = 1500000, moneda = "MXN"), "dani")
  c2 <- requisiciones_carpeta_crear(CID, list(nombre = "Mantenimiento", color = "#b8860b"), "eli")
  r1 <- requisicion_crear(CID, list(titulo = "Cemento y varilla para la cimentación de la nave 2", empresa = "NCS",
    proveedor = "Cemex", monto = 185000, moneda = "MXN", prioridad = 9, carpeta_id = c1,
    notas = "La obra arranca el lunes; sin cemento se detiene la cuadrilla."), "dani")
  requisicion_crear(CID, list(titulo = "Luz bodega 3", empresa = "NCS", proveedor = "CFE", monto = 18000,
    moneda = "MXN", provision_id = "p-luz", carpeta_id = c2), "eli")
  requisicion_crear(CID, list(titulo = "Renta noviembre", empresa = "NTS", proveedor = "Inmobiliaria Sur",
    provision_id = "p-renta"), "eli")
  r4 <- requisicion_crear(CID, list(titulo = "Extintores", empresa = "NTS", proveedor = "Seguridad Total",
    monto = 12000, moneda = "MXN", prioridad = 4, carpeta_id = c2), "dani")
  r5 <- requisicion_crear(CID, list(titulo = "Sillas de oficina (cotización vieja)", empresa = "NCS", monto = 9000,
    moneda = "MXN"), "eli")
  requisiciones_papelera(CID, r5$id, "eli")
  requisicion_crear(CID, list(titulo = "Papelería octubre", empresa = "NCS", provision_id = "p-pagada"), "dani")
  img <- magick::image_write(magick::image_blank(320, 180, "steelblue"), format = "png")
  f <- tempfile(fileext = ".png"); writeBin(img, f)
  evidencia_adjuntar(CID, r1$id, adjunto_preparar(f, "cotizacion_cemex.png", "pegado"), "dani")
  evidencia_tabla_agregar(CID, r1$id, tabla_pegada_preparar(
    "Concepto\tCantidad\tPrecio\nCemento gris 50 kg\t400\t245.00\nVarilla 3/8\t1,200\t98.50"),
    "Cotización Cemex", TRUE, "dani")
  requisicion_comentar(CID, r1$id, "¿Ya tiene fecha de pago? La obra arranca el lunes.", "caro")
  invisible(NULL)
}
if (!identical(Sys.getenv("REQ_VACIO"), "1")) suppressMessages(sembrar())
assign(".req_falla_lectura", identical(Sys.getenv("REQ_FALLA_LECTURA"), "1"), envir = globalenv())
arranque_null <- identical(Sys.getenv("HD_ARRANQUE_NULL"), "1")

nivel   <- Sys.getenv("REQ_NIVEL", "compras")
usuario <- Sys.getenv("REQ_USUARIO", "dani")

ui <- bslib::page_fillable(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA"),
  padding = 0,
  htmltools::div(class = "d-flex gap-2 px-2 pt-1", style = "font-size:.7rem;",
    actionLink("otra_sesion", "Otra sesión: crea una requisición"),
    actionLink("otra_sesion_paga", "Otra sesión: confirma el pago de Luz bodega 3")),
  requisicionesUI("req")
)

server <- function(input, output, session) {
  cid_rv <- reactiveVal(if (arranque_null) NULL else CID)
  mapa_rv <- reactiveVal(if (arranque_null) NULL else c(NCS = "Networks Crossdocking Services", NTS = "Networks Trucking Services"))
  shared <- list(
    effective_client_id = reactive(cid_rv()),
    usuario = reactive(list(user = usuario, name = usuario, tier = nivel, is_staff = FALSE,
                            permisos = permisos_resueltos(nivel, if (nzchar(Sys.getenv("REQ_OVERRIDES"))) Sys.getenv("REQ_OVERRIDES") else "{}"))),
    company_map = reactive(mapa_rv()),
    currency_choices = list(AP = reactiveVal(c("MXN", "USD")), AR = reactiveVal(c("MXN", "USD"))),
    proveedores_db = reactiveVal(NULL)
  )
  mod <- requisicionesServer("req", shared, activa = reactive(TRUE))
  observeEvent(input$otra_sesion, {
    requisicion_crear(CID, list(titulo = "Creada en otra sesión", empresa = "NTS", monto = 500, moneda = "MXN"), "eli")
    .reqm_tocar(CID)
  })
  observeEvent(input$otra_sesion_paga, {
    k <- paste0(CID, "/", S3_KEYS$bancos_confirmados)
    conf <- .req_mem[[k]]
    conf <- dplyr::bind_rows(conf, .normalize(tibble::tibble(confirmacion_id = "c2", provision_id = "p-luz",
      importe = 18450.5, moneda = "MXN", eliminado = FALSE, action = "confirmed", confirmado_at = Sys.time()),
      .schema_bancos_confirmados))
    assign(k, conf, envir = .req_mem)
    .reqm_tocar(CID)
  })
}

app <- shinyApp(ui, server)
local({
  repo_www <- shiny:::staticHandler(file.path(raiz, "www"))
  propio   <- app$httpHandler
  app$httpHandler <<- function(req) repo_www(req) %||% propio(req)
})
app
