# =============================================================================
# tests/test_fixtures_completas.R
# T53: every browser-test fixture (tests/helpers/*_fixture_app.R) starts with
# every function its app calls -- without Chrome.
#
# Fixtures take single functions from R/ with cargar(). When a module starts
# calling a new global, the fixture lacks it, the table's datos() fails and
# the browser tests go red with a cascade nobody reads as "a function is
# missing": T47 (ic_empresas_de) broke 5 of them, T52d (etiquetas_por_factura)
# and T52 (the policies' helpers) 8 fixtures more, all found weeks later.
#
# What this does: each fixture runs in its own R process under
# shiny::testServer() (no browser, no port), its startup graph is flushed (and,
# for Flujo, the switch to Base), and any "could not find function" /
# "object '...' not found" -- from the engine's notice, a module's own
# message, a warning or an error -- fails, naming fixture and function.
# Zero, and no list of exceptions: it measures what runs, not what is named.
#
# Self-check: a copy of the Vencidos fixture without ic_empresas_de, and one
# without etiquetas_por_factura, must be caught (the T47 and T52d cases).
#
# No network (AWS keys emptied, proxy to 127.0.0.1:9) and never prod
# (HOPDESK_PRODUCTION is never set). About 2 minutes.
#
# Run from project root:  Rscript tests/test_fixtures_completas.R
# =============================================================================
cat("=== test_fixtures_completas ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

repo <- normalizePath(".", winslash = "/")

# What a fixture does after its startup, when the function that breaks is only
# reached later. Inputs are the app's (namespaced) ids; `leer` are outputs to
# render.
ACCIONES <- list(
  flujo = list(inputs = list(`cfp-btn_curmode` = 1), leer = "cfp-fx_section_ui")   # Base: the FX row (monedas fase 2)
)

# One fixture in its own process. Returns list(res = "ok" | "ERROR: ...",
# faltan = the "missing" messages seen).
correr_fixture <- function(archivo, accion = NULL) {
  tryCatch(callr::r(function(repo, archivo, accion) {
    setwd(repo)
    Sys.setenv(FLU_DATOS = "prueba", CUENTAS_DIR = tempfile("cuentas_"), HOPDESK_APP_URL = "http://127.0.0.1:9")
    faltan <- character()
    nota <- function(x) if (grepl("could not find function|object '[^']+' not found", x)) faltan <<- c(faltan, x)
    # testServer() evaluates its block inside the app's server, where a
    # fixture's own names win (vencidos has an `accion`): the guard's live in
    # the global environment under names no fixture uses.
    assign(".T53_ACCION", accion, envir = globalenv())
    assign(".T53_NOTA", nota, envir = globalenv())
    res <- withCallingHandlers(tryCatch({
      app <- source(archivo)$value
      shiny::testServer(app, {
        session$flushReact(); session$elapse(1000); session$flushReact()
        if (!is.null(.T53_ACCION)) {
          do.call(session$setInputs, .T53_ACCION$inputs); session$flushReact()
          for (.t53_o in .T53_ACCION$leer)
            tryCatch(output[[.t53_o]], error = function(e) .T53_NOTA(conditionMessage(e)))
        }
      })
      "ok"
    }, error = function(e) { nota(conditionMessage(e)); paste("ERROR:", conditionMessage(e)) }),
    message = function(m) { nota(conditionMessage(m)); invokeRestart("muffleMessage") },
    warning = function(w) { nota(conditionMessage(w)); invokeRestart("muffleWarning") })
    list(res = res, faltan = unique(trimws(faltan)))
  }, args = list(repo = repo, archivo = archivo, accion = accion), timeout = 90,
     env = c(callr::rcmd_safe_env(), AWS_ACCESS_KEY_ID = "", AWS_SECRET_ACCESS_KEY = "", AWS_SESSION_TOKEN = "",
             http_proxy = "http://127.0.0.1:9", https_proxy = "http://127.0.0.1:9", no_proxy = "127.0.0.1,localhost")),
    error = function(e) list(res = paste("ERROR:", conditionMessage(e)), faltan = character()))
}

# ── Self-check: the guard sees the T47 and T52d cases ─────────────────────────
ven <- file.path(repo, "tests", "helpers", "vencidos_fixture_app.R")
quitar <- function(texto, nombre) {
  linea <- grep('cargar("R/data_pipeline.R"', texto, fixed = TRUE)
  stopifnot(length(linea) >= 1L)
  texto[linea] <- gsub(sprintf('"%s", ?', nombre), "", texto[linea], fixed = FALSE)
  texto[linea + 1L] <- gsub(sprintf('"%s", ?', nombre), "", texto[linea + 1L], fixed = FALSE)
  texto
}
for (caso in c("ic_empresas_de", "etiquetas_por_factura")) {
  copia <- tempfile(sprintf("vencidos_sin_%s_", caso), fileext = ".R")
  writeLines(quitar(readLines(ven, warn = FALSE, encoding = "UTF-8"), caso), copia, useBytes = TRUE)
  r <- correr_fixture(copia)
  unlink(copia)
  ok(sprintf("contraprueba: Vencidos without %s is caught", caso),
     any(grepl(sprintf('"%s"', caso), r$faltan, fixed = TRUE)))
}

# ── Every fixture, in zero ─────────────────────────────────────────────────────
fixtures <- sort(list.files(file.path(repo, "tests", "helpers"), "^[a-z].*_fixture_app\\.R$", full.names = TRUE))
ok(sprintf("finds the fixtures (%d)", length(fixtures)), length(fixtures) >= 20L)
for (f in fixtures) {
  nombre <- sub("_fixture_app\\.R$", "", basename(f))
  t0 <- Sys.time()
  r <- correr_fixture(f, ACCIONES[[nombre]])
  dt <- as.numeric(Sys.time() - t0, units = "secs")
  limpio <- identical(r$res, "ok") && !length(r$faltan)
  ok(sprintf("%s starts with every function it calls (%.1fs)", nombre, dt), limpio)
  if (!limpio) {
    if (!identical(r$res, "ok")) cat("      ", substr(r$res, 1, 200), "\n")
    if (length(r$faltan)) cat(paste0("       ", substr(r$faltan, 1, 200)), sep = "\n")
  }
}

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
