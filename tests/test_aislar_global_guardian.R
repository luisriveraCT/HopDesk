# =============================================================================
# tests/test_aislar_global_guardian.R
# A test that loads the WHOLE of R/global.R takes the single-instance lock
# (%LOCALAPPDATA%\HopDesk, keyed on client and bucket) and, with the dev
# .Renviron files, copies the user accounts from the prod bucket into dev.
# With the shared key, tests failed on the lock (another session's test, or
# Luis's local dev app, held it) and a running test kept the dev app from
# starting; with network, they read prod (candado, 2026-11-11).
#   1. every file under tests/ that loads global.R whole -- shiny::loadSupport,
#      or source / sys.source / sourceUTF8 / runApp / shinyAppDir on app.R or
#      R/global.R -- sources tests/helpers/aislar_global.R, unless it is on
#      the list below with its reason; the list stays honest;
#   2. the helper gives this process its own LOCALAPPDATA and no network to
#      AWS, still so after the .Renviron files are read as global.R reads them.
#
# Run from project root:  Rscript tests/test_aislar_global_guardian.R
# =============================================================================
cat("=== test_aislar_global_guardian ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# Files allowed to load global.R whole without the helper, and why.
LISTA_BLANCA <- c(
  "helpers/arranque_app.R" = "T15: its own LOCALAPPDATA (a temp folder) and S3 writes refused in its process, by itself")

# The function name of a call: f, pkg::f or pkg:::f.
nombre_fn <- function(x) {
  f <- x[[1]]
  if (is.name(f)) return(as.character(f))
  if (is.call(f) && as.character(f[[1]]) %in% c("::", ":::")) return(as.character(f[[3]]))
  ""
}
literales <- function(x) {
  if (is.character(x)) return(x)
  if (!is.call(x)) return(character(0))
  unlist(lapply(as.list(x)[-1], literales))
}
carga_global <- function(x) {
  if (!is.call(x)) return(FALSE)
  fn <- nombre_fn(x)
  if (fn == "loadSupport") return(TRUE)
  # The app's app.R or R/global.R, written whole or in file.path() pieces --
  # not another app.R (dev/hd_table_demo/app.R is the engine's demo).
  if (fn %in% c("source", "sys.source", "sourceUTF8", "runApp", "shinyAppDir") && length(x) >= 2L &&
      any(grepl("^(\\./)?(app\\.R|global\\.R|R/global\\.R)$", literales(x[[2]])))) return(TRUE)
  any(vapply(as.list(x), carga_global, logical(1)))
}
usa_helper <- function(x) {
  if (!is.call(x)) return(FALSE)
  if (nombre_fn(x) == "source" && length(x) >= 2L && any(grepl("aislar_global\\.R$", literales(x[[2]])))) return(TRUE)
  any(vapply(as.list(x), usa_helper, logical(1)))
}

archivos <- c(list.files("tests", "\\.R$", full.names = TRUE), list.files("tests/helpers", "\\.R$", full.names = TRUE))
nombre <- function(a) sub("^tests/", "", a)
cargan <- character(0); con_helper <- character(0); sin_parsear <- character(0)
for (a in archivos) {
  ex <- tryCatch(parse(a, keep.source = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(ex)) { sin_parsear <- c(sin_parsear, nombre(a)); next }
  if (any(vapply(as.list(ex), carga_global, logical(1)))) cargan <- c(cargan, nombre(a))
  if (any(vapply(as.list(ex), usa_helper, logical(1)))) con_helper <- c(con_helper, nombre(a))
}

ok("todos los archivos de tests/ se pueden leer",
   { if (length(sin_parsear)) message("  sin parsear: ", paste(sin_parsear, collapse = ", ")); !length(sin_parsear) })
ok(sprintf("el detector encuentra quién carga global.R completo (%d archivos)", length(cargan)),
   all(c("test_autocarga_shiny.R", "test_pasivos_fecha_requerida.R", "test_pronosticos_una_version.R",
         "helpers/tiers_fixture_app.R", "helpers/arranque_app.R") %in% cargan))
faltan <- setdiff(cargan, c(con_helper, names(LISTA_BLANCA), "helpers/aislar_global.R"))
ok("toda prueba que carga global.R completo usa tests/helpers/aislar_global.R",
   { if (length(faltan)) message("  sin el helper: ", paste(faltan, collapse = ", ")); !length(faltan) })
viejos <- setdiff(names(LISTA_BLANCA), cargan)
ok("la lista blanca está al día (cada archivo existe y todavía carga global.R)",
   { if (length(viejos)) message("  sáquelos de la lista: ", paste(viejos, collapse = ", ")); !length(viejos) })

# ── 2. What the helper does, after .Renviron is read as global.R reads it ────
lad_antes <- Sys.getenv("LOCALAPPDATA")
source("tests/helpers/aislar_global.R")
if (file.exists(".Renviron")) readRenviron(".Renviron")
if (file.exists(".Renviron.local")) readRenviron(".Renviron.local")
lad <- normalizePath(Sys.getenv("LOCALAPPDATA"), winslash = "/", mustWork = FALSE)
ok("LOCALAPPDATA es una carpeta de este proceso (su propio candado), no la del usuario",
   startsWith(lad, normalizePath(tempdir(), winslash = "/")) && !identical(lad, normalizePath(lad_antes, winslash = "/", mustWork = FALSE)))
ok("sin red hacia AWS: una petición a S3 falla al conectar (proxy muerto), no responde AWS",
   aislar_global_sin_red())
ok("localhost sigue fuera del proxy (Chrome y las apps de fondo)",
   grepl("127.0.0.1", Sys.getenv("no_proxy"), fixed = TRUE) && grepl("localhost", Sys.getenv("no_proxy"), fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
