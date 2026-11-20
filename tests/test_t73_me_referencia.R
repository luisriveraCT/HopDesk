# =============================================================================
# tests/test_t73_me_referencia.R
# T73b, 2026-11-20. "Guardar como provisión" (the Agregar modal, R/
# staging_browse_module.R + R/manual_entry_handlers.R) saved the provision's
# referencia from input$me_referencia, which no input creates: what the user
# wrote in the modal's field (me_factura, labelled Concepto since T73) was
# lost, and the bank's concepto came out empty. Now it is saved, and the
# audit (provision.generated) carries it.
#
#   1. Every input$me_* that the handler reads is created by the modal.
#   2. On testServer and the in-memory S3: the text written is the saved
#      provision's referencia, and the audit row has it.
#
# Contraprueba: PMOD_R=<old R/pasivos_module.R> must fail 1 and 2.
# Run from project root:  Rscript tests/test_t73_me_referencia.R
# =============================================================================
cat("=== test_t73_me_referencia ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
source("tests/helpers/s3_memoria.R")
source("tests/helpers/aislar_global.R")   # own instance lock, no network: before global.R loads
Sys.unsetenv("HOPDESK_PRODUCTION")
suppressWarnings(suppressMessages(shiny::loadSupport(".", renv = globalenv(), globalrenv = NULL)))
stopifnot("global.R se cargó con red hacia AWS" = aislar_global_sin_red())
assign(".i18n_dir", file.path(getwd(), "www", "i18n"), envir = globalenv())
PMOD <- Sys.getenv("PMOD_R", "R/pasivos_module.R")
if (!identical(PMOD, "R/pasivos_module.R")) suppressMessages(sys.source(PMOD, envir = globalenv()))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# ── 1. The handler only reads inputs the modal creates ─────────────────────
leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
src  <- leer(PMOD)
ini  <- regexpr("observeEvent(input$me_save_as_provision", src, fixed = TRUE)
fin  <- regexpr("observeEvent(input$pasivos_cell_click", src, fixed = TRUE)
ok("the handler of me_save_as_provision is found", ini > 0 && fin > ini)
cuerpo <- substr(src, ini, fin)
leidos <- unique(regmatches(cuerpo, gregexpr("input\\$me_[a-z_]+", cuerpo))[[1]])
leidos <- sub("^input\\$", "", leidos)
modal  <- paste(leer("R/manual_entry_handlers.R"), leer("R/staging_browse_module.R"))
creados <- unique(gsub("[\"']", "", regmatches(modal, gregexpr("[\"']me_[a-z_]+[\"']", modal))[[1]]))
faltan <- setdiff(leidos, creados)
if (length(faltan)) cat("   read but never created:", paste(faltan, collapse = ", "), "\n")
ok("every input$me_* it reads is an input of the modal (me_referencia was not)",
   length(leidos) > 0 && !length(faltan))

# ── 2. The Concepto written is the provision's referencia, and audited ─────
CID <- "acme"
k_prov  <- paste0(CID, "/", S3_KEYS$pasivos_provisions)
k_audit <- paste0(CID, "/", S3_KEYS$pasivos_audit)
has_capability <- function(user, capability, empresa = NULL) TRUE
pasivos_autoriza <- function(capability) TRUE
s3_memoria_reset()
assign(k_prov, .schema_pasivos_provision(), envir = .mem)
G <- new.env()
G$shared <- list(
  effective_client_id = function() CID, current_user = reactiveVal("tester"),
  company_map = reactive(c(NTS = "Networks Transport")), empresa_sel = reactiveVal("Networks Transport"),
  pasivos_provisions_db = reactiveVal(.schema_pasivos_provision()),
  pasivos_liabilities_db = reactiveVal(.schema_pasivos_liability()),
  manual_inv = reactiveVal(.schema_manual()), pagar_hoy_db = reactiveVal(NULL),
  bancos_confirmados = reactiveVal(NULL), visible_initials = reactiveVal(NULL),
  proveedores_db = reactiveVal(NULL), suppress_ledger_prov_refresh = reactiveVal(FALSE))
CONCEPTO <- "Renta noviembre F-77"
servidor <- function(input, output, session) setup_pasivos_module(input, output, session, G$shared)
err <- tryCatch(suppressWarnings(suppressMessages(testServer(servidor, {
  session$flushReact()
  session$setInputs(me_fecha = Sys.Date() + 5, me_empresa = "NTS", me_parte = "Proveedor", me_codigo = "",
                    me_moneda = "MXN", me_importe = 50, me_documento = "D-9", me_factura = CONCEPTO, me_notas = "")
  session$setInputs(me_save_as_provision = 1)
}))), error = function(e) conditionMessage(e))
if (!is.null(err)) message("  testServer: ", err)

p <- get0(k_prov, envir = .mem)
ok("one provision is saved", is.data.frame(p) && nrow(p) == 1L)
ok("its referencia is the Concepto the user wrote (it was \"\")",
   is.data.frame(p) && identical(as.character(p$referencia[1]), CONCEPTO))
a <- get0(k_audit, envir = .mem)
gen <- if (is.data.frame(a)) a[a$action_type %in% "provision.generated", , drop = FALSE] else NULL
ok("the audit row provision.generated carries the Concepto",
   NROW(gen) == 1L && grepl(CONCEPTO, gen$payload_after[1], fixed = TRUE))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
