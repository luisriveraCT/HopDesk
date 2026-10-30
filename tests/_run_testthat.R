# =============================================================================
# tests/_run_testthat.R
# Runs every testthat-style test file (library(testthat) at the top), each in
# its own fresh Rscript process so one file's stubs never leak into another.
# Browser tests (*_chromote.R) need Chrome and are left out.
# Prints one "  PASS  <file>" / "  FAIL  <file>" line per file for CI.
# Run from the repo root: Rscript tests/_run_testthat.R
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

archivos <- list.files("tests", "^test_.*[.]R$", full.names = TRUE)
archivos <- archivos[!grepl("_chromote[.]R$", archivos)]
archivos <- Filter(function(f) any(grepl("^[^#]*library[(]testthat[)]", readLines(f, warn = FALSE))), archivos)

rscript <- file.path(R.home("bin"), "Rscript")
fallas <- 0L
for (f in archivos) {
  salida <- suppressWarnings(system2(rscript, c("--vanilla", shQuote(f)), stdout = TRUE, stderr = TRUE))
  estado <- attr(salida, "status") %||% 0L
  if (identical(as.integer(estado), 0L)) {
    cat(sprintf("  PASS  %s\n", f))
  } else {
    fallas <- fallas + 1L
    cat(sprintf("  FAIL  %s\n", f))
    cat(paste0("        ", utils::tail(salida, 25)), sep = "\n")
  }
}
cat(sprintf("\n%d archivo(s), %d con fallas\n", length(archivos), fallas))
if (fallas) quit(status = 1)
