# =============================================================================
# tests/helpers/proveedores_testserver.R
# Runs the REAL proveedoresServer (R/proveedores_module.R) in shiny::testServer
# over in-memory provider lists -- no S3, no browser -- and drives the Excel
# import the way a user does: the file input, the conflict buttons, the
# "incluir" checkboxes and Importar. save_proveedores() and
# save_proveedores_inactivos() are replaced in this process by functions that
# keep what would be saved.
#
#   r <- prov_importar("tests/fixtures/proveedores_excel/01_reemplazo.xlsx",
#                      activos = df, decision = "reemplazar")
#   r$filas      # the classified rows (upload_data_rv()$rows), NULL if unread
#   r$activos    # what Importar saved as the active list (NULL: not saved)
#   r$inactivos  # what it saved as the inactive list   (NULL: not saved)
#   r$avisos     # showNotification() texts, in order
#
# PROV_RAIZ: the checkout whose R/ to use (default: this one).
# =============================================================================
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(readxl) })
.prov_raiz <- normalizePath(Sys.getenv("PROV_RAIZ", "."), winslash = "/")
local({
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
  assign("%||%", `%||%`, envir = globalenv())
  cargar <- function(archivo, nombres) {
    ex <- parse(file.path(.prov_raiz, archivo), keep.source = FALSE)
    for (e in ex)
      if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
          as.character(e[[2]]) %in% nombres)
        eval(e, envir = globalenv())
  }
  cargar("R/global.R", c("strip_accents", "CURRENCIES"))
  cargar("R/settings/settings_helpers.R", ".BANCOS_MX_CHOICES")
  cargar("R/proveedores_match.R", "parse_activo_hasta")
  cargar("R/persistence.R", c(".schema_proveedores", ".normalize"))
  owd <- setwd(.prov_raiz); on.exit(setwd(owd))
  for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/hd_columnas.R", "R/hd_table.R",
              "R/proveedores_excel.R", "R/proveedores_module.R"))
    if (file.exists(f)) sys.source(f, envir = globalenv())
  assign(".i18n_dir", file.path(.prov_raiz, "www", "i18n"), envir = globalenv())
})

.prov_guardado <- new.env()
assign("bump_sync_version", function(...) invisible(NULL), envir = globalenv())
assign("puede", function(usuario, permiso, ...) TRUE, envir = globalenv())
assign("puede_o_avisa", function(...) TRUE, envir = globalenv())
assign("log_action", function(...) invisible(NULL), envir = globalenv())
assign("save_proveedores", function(df, client_id = NULL) assign("activos", df, envir = .prov_guardado),
       envir = globalenv())
assign("save_proveedores_inactivos", function(df, client_id = NULL) assign("inactivos", df, envir = .prov_guardado),
       envir = globalenv())

# One provider row with every field the catalog keeps.
prov_fila <- function(id, nombre, no_cuenta, banco = "012", alias = "", rfc = "", correo = "", moneda = "MXN",
                      empresas = "NTS", codigo = "", tipo_relacion = "", activo = TRUE,
                      activo_hasta = "indefinido", created_at = "2025-01-15 10:00:00", clabe = no_cuenta)
  .normalize(data.frame(id = id, Empresa = sub(",.*", "", empresas), codigo = codigo, nombre = nombre,
                        alias = alias, clabe = clabe, medio_pago = "SPI", rfc = rfc, tipo = "012",
                        banco_destino = banco, activo = activo, no_cuenta = no_cuenta, banco = banco,
                        correo = correo, moneda = moneda, tipo_relacion = tipo_relacion, empresas = empresas,
                        status = "completo", fuente = "manual", upload_batch = NA_character_,
                        activo_hasta = activo_hasta, created_at = created_at, updated_at = created_at,
                        stringsAsFactors = FALSE), .schema_proveedores)

prov_importar <- function(archivo, activos = prov_fila("p-0", "SIN USO", "000000000000000000")[0, ],
                          inactivos = activos[0, ], tipo = "activos", empresa = "TODOS",
                          decision = NULL, incluir_incompletos = FALSE, incluir_duplicados = FALSE,
                          importar = TRUE) {
  rm(list = ls(.prov_guardado), envir = .prov_guardado)
  avisos <- character(0)
  out <- NULL
  shared <- list(
    proveedores_db           = reactiveVal(activos),
    proveedores_inactivos_db = reactiveVal(inactivos),
    company_map              = reactive(c(NTS = "Networks Trucking", NCS = "Networks Crossdocking")),
    effective_client_id      = function() "prueba",
    home_client_id           = function() "prueba",
    current_user             = function() "prueba",
    usuario                  = function() "prueba"
  )
  # Keep the notifications' text (the module runs in globalenv, so this one
  # is found first); put things back as they were on the way out.
  previo <- if (exists("showNotification", envir = globalenv(), inherits = FALSE)) get("showNotification", envir = globalenv())
  on.exit(if (is.null(previo)) rm("showNotification", envir = globalenv()) else assign("showNotification", previo, envir = globalenv()),
          add = TRUE)
  assign("showNotification", function(ui, ...) { avisos <<- c(avisos, paste(as.character(ui), collapse = " ")); invisible(NULL) },
         envir = globalenv())
  shiny::testServer(proveedoresServer, args = list(shared = shared), {
    session$setInputs(excel_tipo = tipo, excel_empresa = empresa)
    session$setInputs(excel_file = data.frame(name = basename(archivo), size = file.size(archivo),
                                              type = "", datapath = normalizePath(archivo), stringsAsFactors = FALSE))
    ud <- upload_data_rv()
    filas <- if (!is.null(ud)) ud$rows
    if (!is.null(ud) && importar) {
      if (identical(decision, "reemplazar")) session$setInputs(conf_reemplazar_todos = 1)
      else if (!is.null(decision)) {
        boton <- c(conservar = "conf_conservar", omitir = "conf_omitir")[[decision]]
        for (k in seq_len(sum(grepl("^conflicto", filas$.clasificacion))))
          do.call(session$setInputs, setNames(list(k), boton))
      }
      if (incluir_incompletos) session$setInputs(incluir_incompletos = TRUE)
      if (incluir_duplicados)  session$setInputs(incluir_duplicados = TRUE)
      session$setInputs(confirmar_upload = 1)
    }
    out <<- list(filas = filas)
  })
  c(out, list(activos = .prov_guardado$activos, inactivos = .prov_guardado$inactivos, avisos = avisos))
}
