# =============================================================================
# tests/helpers/s3_red_harness.R
# For tests that extract .s3_write() from R/persistence.R one function at a
# time (instead of sourcing the file): also load the CM09 write net it now
# calls, and the caches it reads, so .s3_write() behaves as in the app.
# Source it right after extracting .s3_write().
# =============================================================================
local({
  piezas <- c(".s3_red_revisar", ".s3_motivo_reduccion", ".S3_SOLO_CRECEN", ".S3_SIN_RED",
              ".s3_sin_red", ".s3_filas_registrar", ".s3_filas_vigente", ".s3_leer_para_escribir", ".s3_existe",
              ".s3_existe_full", ".s3_missing_si_no_existe", ".s3_missing_cache_hit",
              ".s3_missing_cache_set")
  for (e in parse("R/persistence.R"))
    if (is.call(e) && identical(e[[1]], as.name("<-")) &&
        as.character(e[[2]]) %in% piezas && !exists(as.character(e[[2]]), envir = globalenv()))
      assign(as.character(e[[2]]), eval(e[[3]], envir = globalenv()), envir = globalenv())
  for (env_name in c(".s3_filas", ".s3_missing_cache", ".s3_preload_cache"))
    if (!exists(env_name, envir = globalenv())) assign(env_name, new.env(parent = emptyenv()), envir = globalenv())
  if (!exists(".S3_MISSING_CACHE_TTL", envir = globalenv())) assign(".S3_MISSING_CACHE_TTL", 60, envir = globalenv())
  if (!exists(".S3_FILAS_TTL", envir = globalenv())) assign(".S3_FILAS_TTL", 60, envir = globalenv())
  if (!exists("S3_KEYS", envir = globalenv()))
    for (e in parse("R/global.R"))
      if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(as.character(e[[2]]), "S3_KEYS"))
        eval(e, globalenv())
  if (!exists(".s3_read_with", envir = globalenv()))
    for (e in parse("R/persistence.R"))
      if (is.call(e) && identical(e[[1]], as.name("<-")) &&
          as.character(e[[2]]) %in% c(".s3_read_with", ".s3_read_raw_with_retry"))
        assign(as.character(e[[2]]), eval(e[[3]], envir = globalenv()), envir = globalenv())
})
