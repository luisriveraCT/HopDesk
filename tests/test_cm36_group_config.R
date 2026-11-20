# =============================================================================
# tests/test_cm36_group_config.R
# CM36. Saving "Configuración del grupo" (Ajustes › Empresas) wrote the
# session's whole copy of group_config.rds (save_group_config()): two
# sessions that changed different fields overwrote each other's, and a
# session whose load had failed held the empty schema and, on Guardar, wiped
# the logo and the base currency. Now:
#   1  modificar_group_config(): strict read (a failed read writes nothing),
#      no file = the empty schema, nothing changed = no write, a non-list
#      file is never replaced;
#   2  .group_config_rebase(): three sessions from the same copy change the
#      name, the base currency and the logo -- all three survive; the logo's
#      bytes and extension are one unit (never one session's bytes with
#      another's extension); fields this form does not know stay S3's;
#   3  the real observeEvent(input$btn_save_group_config) of empresasServer
#      (testServer) over the in-memory S3.
# Run from project root:  Rscript tests/test_cm36_group_config.R
# =============================================================================
cat("=== test_cm36_group_config ===\n\n")
source("tests/helpers/s3_memoria.R")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

CID <- "acme"
KEY <- paste0(CID, "/", S3_KEYS$group_config)
poner <- function(x) assign(KEY, x, envir = .mem)
leer  <- function() get(KEY, envir = .mem)
hay   <- function() exists(KEY, envir = .mem, inherits = FALSE)
LOGO_A <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 1)); LOGO_B <- as.raw(c(0xff, 0xd8, 0xff, 2)); LOGO_C <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 3))
semilla <- list(group_name = "Grupo Acme", logo_raw = LOGO_A, logo_ext = "png", moneda_base = "MXN")
guardar <- function(nuevo, base)   # what Empresas' Guardar does
  suppressMessages(modificar_group_config(function(fresco) .group_config_rebase(nuevo, base, fresco), CID))

# ── 1. modificar_group_config() ──────────────────────────────────────────────
s3_memoria_reset()
r <- suppressMessages(modificar_group_config(function(f) { f$group_name <- "Nuevo"; f }, CID))
ok("1a no file yet: it is created from the empty schema with the change", hay() &&
     identical(leer()$group_name, "Nuevo") && is.na(leer()$moneda_base) && identical(r, leer()))

s3_memoria_reset(); poner(semilla); n0 <- .mem_escrituras
suppressMessages(modificar_group_config(function(f) f, CID))
ok("1b nothing changed: nothing written", .mem_escrituras == n0)

.mem_falla <- KEY; n0 <- .mem_escrituras
e <- tryCatch(suppressMessages(modificar_group_config(function(f) { f$group_name <- "X"; f }, CID)), error = function(e) e)
.mem_falla <- character()
ok("1c a failed read stops and writes nothing (S3 keeps the logo and the currency)",
   inherits(e, "error") && .mem_escrituras == n0 && identical(leer(), semilla))

poner("no soy una lista"); n0 <- .mem_escrituras
e <- tryCatch(suppressMessages(modificar_group_config(function(f) { f$group_name <- "X"; f }, CID)), error = function(e) e)
ok("1d a file that is not a list is never replaced", inherits(e, "error") && .mem_escrituras == n0 &&
     identical(leer(), "no soy una lista"))

e <- tryCatch(modificar_group_config(function(f) f, ""), error = function(e) e)
ok("1e no client_id: it stops", inherits(e, "error"))

old <- list(group_name = "Viejo", logo_raw = NULL, logo_ext = "png")   # a file from before moneda_base
s3_memoria_reset(); poner(old)
suppressMessages(modificar_group_config(function(f) { f$group_name <- "Nuevo"; f }, CID))
ok("1f an older file without moneda_base gets it as NA, and keeps its other fields",
   identical(leer()$group_name, "Nuevo") && is.na(leer()$moneda_base) && identical(leer()$logo_ext, "png"))

# ── 2. Three sessions from the same copy ─────────────────────────────────────
s3_memoria_reset(); poner(semilla)
base <- leer()                                    # A, B and C load the same config
a <- base; a$group_name <- "Grupo Acme SA"
b <- base; b$moneda_base <- "USD"
c <- base; c$logo_raw <- LOGO_B; c$logo_ext <- "jpg"
guardar(a, base); guardar(b, base); guardar(c, base)
g <- leer()
ok("2a A's name, B's base currency and C's logo all survive",
   identical(g$group_name, "Grupo Acme SA") && identical(g$moneda_base, "USD") &&
     identical(g$logo_raw, LOGO_B) && identical(g$logo_ext, "jpg"))

# D loaded before C saved (logo A, png) and uploads its own png: D's bytes
# with D's extension -- never D's bytes with C's "jpg" (an unchanged ext alone
# would have kept S3's).
d <- base; d$logo_raw <- LOGO_C; d$logo_ext <- "png"
guardar(d, base)
ok("2b the logo is one unit: the last upload's bytes AND its extension, never crossed",
   identical(leer()$logo_raw, LOGO_C) && identical(leer()$logo_ext, "png"))

# E changes only the name: the logo another session uploaded stays.
s3_memoria_reset(); poner(semilla); base <- leer()
c <- base; c$logo_raw <- LOGO_B; c$logo_ext <- "jpg"; guardar(c, base)
e2 <- base; e2$group_name <- "Otro nombre"; guardar(e2, base)
ok("2c a save that does not touch the logo leaves the one another session uploaded",
   identical(leer()$logo_raw, LOGO_B) && identical(leer()$logo_ext, "jpg") && identical(leer()$group_name, "Otro nombre"))

# A field this form does not know (a later stage's, e.g. a theme) stays S3's.
s3_memoria_reset(); poner(c(semilla, list(tema = "marca"))); base <- semilla   # the session's copy predates it
a <- base; a$group_name <- "Con tema"; guardar(a, base)
ok("2d a field the form does not know stays as S3 holds it", identical(leer()$tema, "marca") &&
     identical(leer()$group_name, "Con tema"))

# The bad case of before: a session whose copy is the empty schema (its load
# failed) saves its name: the logo and the currency in S3 are kept.
s3_memoria_reset(); poner(semilla); base <- .schema_group_config()
a <- base; a$group_name <- "Solo el nombre"; guardar(a, base)
ok("2e a session holding the empty schema changes the name only: S3's logo and currency stay",
   identical(leer()$logo_raw, LOGO_A) && identical(leer()$moneda_base, "MXN") && identical(leer()$group_name, "Solo el nombre"))

ok("2f save_group_config() is gone: no blind whole-copy write left",
   !exists("save_group_config") && !length(grep("save_group_config(", sub("#.*$", "", unlist(lapply(
     c(list.files("R", pattern = "\\.R$", full.names = TRUE, recursive = TRUE), "app.R"), readLines, warn = FALSE))), fixed = TRUE)))

# ── 3. The real Guardar of Empresas ──────────────────────────────────────────
suppressPackageStartupMessages(library(shiny))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/empresas_module.R"))
  suppressMessages(source(f, encoding = "UTF-8"))
.i18n_dir <- file.path(getwd(), "www", "i18n")
puede <- function(usuario, permiso, ...) TRUE
bump_sync_version <- function(...) invisible(NULL)
erp_access_allowed <- function(...) TRUE
sap_creds_source <- function(...) NULL
load_erp_connections <- function(...) .schema_erp_connections()
LOGS <- character()
log_action <- function(..., action = NULL) { LOGS <<- c(LOGS, action %||% "?"); invisible(NULL) }

sesion <- function(copia) {   # one session's guardar: its copy, its form, the click
  res <- NULL
  gc_rv <- reactiveVal(copia)
  shared <- list(
    empresas_db = reactiveVal(.schema_empresas()), company_map = reactive(character()),
    effective_client_id = function() CID, home_client_id = function() CID,
    current_user = function() "luis", usuario = function() "luis",
    current_user_info = reactive(list(tier = "dev", is_staff = FALSE)),
    jump_client_id = function() NULL, visible_initials = function() NULL,
    group_config = gc_rv, sap_data = reactiveVal(NULL),
    moneda_base_cliente = reactive(moneda_base_resolver(gc_rv()$moneda_base)))
  list(gc = gc_rv, correr = function(inputs) {
    testServer(empresasServer, args = list(shared = shared), {
      do.call(session$setInputs, inputs)
      session$setInputs(btn_save_group_config = 1)
      res <<- gc_rv()
    })
    res
  })
}

s3_memoria_reset(); poner(semilla); base <- leer()
sA <- sesion(base); sB <- sesion(base)
rA <- tryCatch(sA$correr(list(group_name_input = "Acme Holding", moneda_base_input = "MXN")),
               error = function(e) { message("  ERROR A: ", conditionMessage(e)); NULL })
rB <- tryCatch(sB$correr(list(group_name_input = "Grupo Acme", moneda_base_input = "EUR")),
               error = function(e) { message("  ERROR B: ", conditionMessage(e)); NULL })
g <- leer()
ok("3a two real sessions: A's name and B's currency both in S3", identical(g$group_name, "Acme Holding") &&
     identical(g$moneda_base, "EUR") && identical(g$logo_raw, LOGO_A))
ok("3b each session's copy becomes what S3 holds now (B sees A's name too)",
   identical(rB$group_name, "Acme Holding") && identical(rB$moneda_base, "EUR"))
ok("3c both saves were logged", sum(LOGS == "guardar_config_grupo") == 2L)

.mem_falla <- KEY; n0 <- .mem_escrituras; LOGS <- character()
sC <- sesion(.schema_group_config())   # its load failed: it holds the empty schema
rC <- tryCatch(sC$correr(list(group_name_input = "Nombre", moneda_base_input = "")), error = function(e) NULL)
.mem_falla <- character()
ok("3d a failed read on Guardar writes nothing and leaves the session's copy as it was",
   .mem_escrituras == n0 && identical(leer(), g) && identical(rC$group_name, "") && !length(LOGS))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
