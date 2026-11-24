# =============================================================================
# tests/helpers/ic_filtro_piezas.R
# T89: the pieces panorama_filas_vigentes() (R/panorama_datos.R) needs, for
# tests that load Panorama piece by piece instead of the whole app: the
# intercompany filter of R/data_pipeline.R (apply_ic_filter,
# build_ic_fullcodes, ic_empresas_de), normalize_code() from
# R/persistence.R, and R/empresas_identidad.R (the filter's no-registry
# fallback). Only those definitions are evaluated: the files' other
# top-level code needs S3 and secrets.
#   source("tests/helpers/ic_filtro_piezas.R")   (a fixture app: after setting `raiz`)
# =============================================================================
local({
  base <- if (exists("raiz", envir = globalenv())) get("raiz", envir = globalenv()) else "."
  tomar <- function(archivo, nombres) {
    for (e in parse(file.path(base, archivo), keep.source = FALSE))
      if (is.call(e) && is.name(e[[1]]) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
          as.character(e[[2]]) %in% nombres)
        eval(e, envir = globalenv())
  }
  tomar("R/persistence.R", "normalize_code")
  tomar("R/data_pipeline.R", c("apply_ic_filter", "build_ic_fullcodes", "ic_empresas_de"))
  if (!exists("empresas_identidad", envir = globalenv()))
    suppressMessages(sys.source(file.path(base, "R/empresas_identidad.R"), envir = globalenv()))
})
