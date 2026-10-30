# Guardian: no tier key written as a literal outside the registry.
# A tier is added in R/tier_registry.R and nowhere else (plus its name in the
# dictionary). Run from the repo root: Rscript tests/test_niveles_guardian.R
library(testthat)
`%||%` <- function(a, b) if (is.null(a)) b else a
tr <- function(clave, ...) clave
source("R/tier_registry.R")

# Files allowed to name tiers: the registries themselves.
PERMITIDOS <- c("R/tier_registry.R", "R/permisos.R")

.sin_comentario <- function(linea) {
  # Drop a trailing comment that is not inside a string (good enough for code).
  dentro <- FALSE; q <- ""
  for (i in seq_len(nchar(linea))) {
    ch <- substr(linea, i, i)
    if (dentro) { if (ch == q && substr(linea, i - 1, i - 1) != "\\") dentro <- FALSE }
    else if (ch %in% c("\"", "'")) { dentro <- TRUE; q <- ch }
    else if (ch == "#") return(substr(linea, 1, i - 1))
  }
  linea
}

test_that("ningún nivel escrito a mano fuera del registro", {
  archivos <- c("app.R", list.files("R", "[.]R$", recursive = TRUE, full.names = TRUE))
  archivos <- setdiff(archivos, PERMITIDOS)
  pat <- paste0("[\"'](", paste(names(TIER_REGISTRY), collapse = "|"), ")[\"']")
  hallazgos <- character()
  for (f in archivos) {
    lineas <- readLines(f, warn = FALSE, encoding = "UTF-8")
    for (i in seq_along(lineas)) {
      codigo <- .sin_comentario(lineas[i])
      if (grepl("nivel-literal-ok", lineas[i], fixed = TRUE)) next
      # A tier literal only matters where tiers are in play.
      if (grepl(pat, codigo, perl = TRUE) && grepl("tier|nivel", codigo, ignore.case = TRUE))
        hallazgos <- c(hallazgos, sprintf("%s:%d: %s", f, i, trimws(lineas[i])))
    }
  }
  expect_length(hallazgos, 0)
  if (length(hallazgos)) message(paste(hallazgos, collapse = "\n"))
})

test_that("el registro es coherente", {
  rangos <- vapply(TIER_REGISTRY, `[[`, integer(1), "rango")
  expect_false(anyDuplicated(rangos) > 0)
  expect_length(nivel_ti(), 1)
  expect_length(nivel_alta(), 1)
  expect_true(all(niveles_invitables() %in% niveles_cliente()))
  expect_false(nivel_ti() %in% niveles_invitables())
  # every tier has its name in both dictionaries
  for (idioma in c("es", "en")) {
    dic <- jsonlite::fromJSON(file.path("www/i18n", paste0(idioma, ".json")))
    faltan <- setdiff(paste0("niveles.nombre.", names(TIER_REGISTRY)), names(dic))
    expect_length(faltan, 0)
  }
})

test_that("un nivel nuevo aparece solo en las listas", {
  viejo <- TIER_REGISTRY
  on.exit(TIER_REGISTRY <<- viejo)
  TIER_REGISTRY$prueba <<- list(rango = 15L, interno = FALSE, color = "#000",
                                invitable = TRUE, ti = FALSE, unico_vivo = FALSE,
                                alta = FALSE, pestanas = NULL, permisos = character(),
                                bloqueados = character())
  expect_true("prueba" %in% niveles_cliente())
  expect_true("prueba" %in% niveles_invitables())
  expect_identical(niveles_cliente()[length(niveles_cliente()) - 1], "prueba")
  expect_true(tier_assignment_allowed("prueba", nivel_ti(), FALSE))
})
