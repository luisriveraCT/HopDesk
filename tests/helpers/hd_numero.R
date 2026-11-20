# tests/helpers/hd_numero.R
# T66: loads hd_numero_input() and its helpers from R/ui_components.R into the
# global environment, for tests and fixtures that load the app piece by piece
# (a screen that draws a number field needs them). Uses `raiz` when the
# fixture defines it. Only these definitions:
# the rest of ui_components.R is not evaluated.
#   source("tests/helpers/hd_numero.R")
local({
  nombres <- c("HD_NUMERO_FORMATOS", ".hd_numero_decimales", "hd_numero_texto",
               "hd_numero_deps", "hd_numero_input", "hd_numero_update")
  hallados <- character()
  archivo <- file.path(get0("raiz", envir = globalenv(), ifnotfound = "."), "R/ui_components.R")
  for (e in parse(archivo, keep.source = FALSE, encoding = "UTF-8"))
    if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% nombres) {
      eval(e, envir = globalenv())
      hallados <- c(hallados, as.character(e[[2]]))
    }
  # hd_numero_texto() formats a monto with monto_moneda() (R/monedas.R)
  if (!exists("monto_moneda", envir = globalenv()))
    for (e in parse(sub("ui_components.R$", "monedas.R", archivo), keep.source = FALSE, encoding = "UTF-8"))
      if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=") && is.name(e[[2]]) &&
          identical(as.character(e[[2]]), "monto_moneda")) eval(e, envir = globalenv())
  faltan <- setdiff(nombres, hallados)
  if (length(faltan)) stop("tests/helpers/hd_numero.R: not in R/ui_components.R: ", paste(faltan, collapse = ", "))
})
