# =============================================================================
# tests/helpers/server_usuarios_archivos.R
# T100. ONE list of what the real tiersServer (Usuarios, and Archivos de pago:
# pagosAuditViewerServer, which tiersServer starts) needs, for every test that
# runs it in shiny::testServer. Each test used to carry its own copy (12 to 18
# files); T93 added R/bancos_respuesta.R to what the server uses and 3 of the
# 4 copies never heard of it: their respuesta_banco table failed on
# BTR_ESTADOS and they still passed (TANDA64 stopped on the 4th).
#
# How a test loads the server:
#   source("tests/helpers/s3_memoria.R")            # S3 in memory, never a bucket
#   source("tests/helpers/server_usuarios_archivos.R")
#   cargar_server_usuarios()
#   ...then its own stand-ins (log_action, IS_ADMIN_DEPLOYMENT, ...), AFTER.
#
# cargar_server_usuarios():
#   1. every FUNCTION definition in R/ (no top-level side effects);
#   2. the files of ARCHIVOS_SERVER_USUARIOS in full, in order: their
#      constants and registries (BTR_ESTADOS, PERMISOS, TIER_REGISTRY, ...),
#      which step 1 does not define.
#
# tests/test_t100_server_usuarios_guardiana.R keeps this list honest: every
# R/ file whose top-level constants the server reaches (transitively) must be
# in ARCHIVOS_SERVER_USUARIOS or in NO_CARGADOS_A_PROPOSITO with its reason.
# A file new to the server fails there, by name, saying where to add it.
# =============================================================================

# Loaded in full, in this order.
ARCHIVOS_SERVER_USUARIOS <- c(
  "R/tier_registry.R", "R/permisos.R", "R/i18n.R", "R/auth.R", "R/tiers_tab_config.R",
  "R/audit_log_viewer_module.R", "R/pagos_audit_viewer_module.R", "R/hd_registro.R",
  "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R", "R/usuarios_cuentas.R",
  "R/tiers_module.R", "R/tema_registro.R", "R/email_service.R", "R/auth_reset.R",
  "R/bancos_respuesta.R",          # T93: Archivos de pago's respuesta_banco table (BTR_ESTADOS)
  # T100: the rest of what the server reaches; only definitions at top level.
  "R/app_audit.R", "R/audit_sticky_ui.R", "R/avisos.R", "R/avisos_alertas_module.R",
  "R/erp_connector_registry.R", "R/offline_queue.R", "R/requisiciones_persistence.R",
  "R/sync_bus.R", "R/ui_components.R"
)

# Reached by the server, NOT loaded here, and why.
NO_CARGADOS_A_PROPOSITO <- c(
  "R/persistence.R" = "tests/helpers/s3_memoria.R loads it (with S3_KEYS) over the in-memory store; every server test sources s3_memoria.R first",
  "R/global.R"      = "never in full: it opens AWS, the instance lock and .Renviron; S3_KEYS comes from s3_memoria.R and each test sets IS_ADMIN_DEPLOYMENT itself"
)

# reemplazos: c("R/tiers_module.R" = "<other path>") loads another copy of a
# file in its place (test_usuarios_cambio_cliente.R's TM_ARCHIVO mutant hook).
cargar_server_usuarios <- function(reemplazos = character()) {
  suppressMessages(for (f in list.files("R", pattern = "[.]R$", recursive = TRUE, full.names = TRUE)) {
    ex <- tryCatch(parse(f, keep.source = FALSE), error = function(e) expression())
    for (e in ex)
      if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
          is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")))
        tryCatch(eval(e, envir = globalenv()), error = function(err) NULL)
  })
  archivos <- ARCHIVOS_SERVER_USUARIOS
  cambia <- archivos %in% names(reemplazos)
  archivos[cambia] <- unname(reemplazos[archivos[cambia]])
  falta <- archivos[!file.exists(archivos)]
  if (length(falta)) stop("cargar_server_usuarios(): missing ", paste(falta, collapse = ", "), call. = FALSE)
  suppressMessages(for (f in archivos) source(f, local = FALSE))
  invisible(archivos)
}
