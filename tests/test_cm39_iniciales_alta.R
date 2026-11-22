# =============================================================================
# tests/test_cm39_iniciales_alta.R
# CM39. Creating a company checked its initials (taken, or another company's
# earlier ones -- CM29) only against the session's copy, and the save is
# rebased by id (CM38b): two sessions creating "NT" at once both passed and
# both were written -- two companies with the same initials, one hiding the
# other in company_map. Now the checks are made again on S3's table inside
# the write's cambio():
#   1  A creates "NT", then B (copy taken before A's) creates "NT": one "NT"
#      only; B gets the clear notice, no log, no sync bump, and B's session
#      now shows A's company;
#   2  the same with " nt " (case and spaces);
#   3  B creates "OLD" while S3 holds a company renamed OLD -> NEW that B's
#      copy never had: refused (iniciales_de_antes), nothing written;
#   4  a plain create still works: "creada" notice, log, sync bump;
#   5  two creates at once with different initials both computed the same
#      account_code on their copies: the second gets the next free one, and
#      its notice and log name the code written (second commit).
# The real empresasServer (testServer), two sessions over the in-memory S3.
# Against 867d251, 1, 2, 3 and 5 fail (both "NT" are written; both get E0003).
# Run from project root:  Rscript tests/test_cm39_iniciales_alta.R
# =============================================================================
cat("=== test_cm39_iniciales_alta ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages({ library(shiny) })
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/empresas_module.R"))
  suppressMessages(source(f, encoding = "UTF-8"))
.i18n_dir <- file.path(getwd(), "www", "i18n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

puede <- function(usuario, permiso, ...) TRUE
erp_access_allowed <- function(...) TRUE
sap_creds_source <- function(...) NULL
load_erp_connections <- function(...) .schema_erp_connections()
rename_empresa_initials <- function(...) invisible(character())
LOGS <- list(); AVISOS <- character(); TIPOS <- character(); BUMPS <- 0L
log_action <- function(..., action = NULL, metadata = NULL, description = NULL) {
  if (identical(action, "sin_evento")) return(invisible(NULL))   # T72 B: the write layer's own entry (test_t72b_evento)
  LOGS[[length(LOGS) + 1L]] <<- list(action = action %||% "?", metadata = metadata, description = description)
  invisible(NULL)
}
showNotification <- function(ui, ..., type = "default") {
  AVISOS <<- c(AVISOS, as.character(ui)); TIPOS <<- c(TIPOS, type); invisible(NULL)
}
bump_sync_version <- function(...) { BUMPS <<- BUMPS + 1L; invisible(NULL) }

emp <- function(id, ini, ant = NA_character_) .normalize(tibble::tibble(
  id = id, account_code = sprintf("E%04d", seq_along(id)), initials = ini, razon_social = paste(ini, "SA"),
  nombre_corto = ini, nombres_alt = "[]", rfc = paste0(ini, "010101AA1"), activa = TRUE, deleted = FALSE,
  created_at = "2026-01-01", updated_at = "2026-01-01", iniciales_anteriores = ant), .schema_empresas)
CID <- "a"
KEY <- paste0(CID, "/", S3_KEYS$empresas)
poner <- function(df) assign(KEY, df, envir = .mem)
leer  <- function() get(KEY, envir = .mem)
sembrar <- function() {
  s3_memoria_reset(); LOGS <<- list(); AVISOS <<- character(); TIPOS <<- character(); BUMPS <<- 0L
  poner(emp(c("a1", "a2"), c("AA", "AB")))
}
limpiar <- function() { LOGS <<- list(); AVISOS <<- character(); TIPOS <<- character(); BUMPS <<- 0L }
# A session's shared: its copy of the companies is read when it starts.
nuevo_shared <- function() {
  db <- reactiveVal(leer())
  list(empresas_db = db, company_map = reactive(.company_map_from_empresas(db())),
       effective_client_id = function() CID, home_client_id = function() CID,
       current_user = function() "ana", usuario = function() "ana",
       current_user_info = reactive(list(tier = "admin", is_staff = FALSE)),
       jump_client_id = function() NULL, visible_initials = function() NULL,
       group_config = reactiveVal(NULL), sap_data = reactiveVal(NULL))
}
crear <- function(sh, ini) testServer(empresasServer, args = list(shared = sh), {
  session$setInputs(new_initials = ini, new_razon_social = paste(trimws(ini), "SA"), new_nombre_corto = "",
                    new_rfc = "", new_nombres_alt = "", new_activa = TRUE)
  session$setInputs(btn_save_new_empresa = as.numeric(Sys.time()))
})
es <- function(k, ...) tr(paste0("tablas.empresas.aviso.", k), ..., idioma = "es")
n_ini <- function(ini) sum(toupper(trimws(leer()$initials)) == ini, na.rm = TRUE)

# ── 1. Two sessions create "NT" at once ──────────────────────────────────────
for (caso in list(list(n = "1", ini = "NT"), list(n = "2", ini = " nt "))) {
  sembrar()
  sa <- nuevo_shared(); sb <- nuevo_shared()             # both copies taken before either saves
  crear(sa, "NT")
  limpiar()
  crear(sb, caso$ini)                                    # B's copy has no "NT"
  ok(sprintf("%sa only one company \"NT\" (B's \"%s\" was not written)", caso$n, caso$ini),
     n_ini("NT") == 1L && nrow(leer()) == 3L)
  ok(sprintf("%sb B gets the clear notice -- and nothing else: no log, no sync bump", caso$n),
     identical(AVISOS, es("iniciales_recien_dadas", iniciales = "NT")) && identical(TIPOS, "error") &&
       !length(LOGS) && BUMPS == 0L)
  if (caso$n == "1")
    ok("1c B's session now shows A's company", "NT" %in% isolate(sb$empresas_db())$initials)
}

# ── 3. Another company's earlier initials, written meanwhile ────────────────
sembrar()
sb <- nuevo_shared()                                     # B's copy: AA, AB
poner(dplyr::bind_rows(leer(), emp("x1", "NEW", '["OLD"]') |> transform(account_code = "E0003")))
n0 <- .mem_escrituras
crear(sb, "OLD")
ok("3 \"OLD\" (another company's earlier initials, in S3 only) is refused: nothing written",
   .mem_escrituras == n0 && n_ini("OLD") == 0L && identical(AVISOS, es("iniciales_de_antes")) &&
     !length(LOGS) && BUMPS == 0L)

# ── 4. A plain create still works ────────────────────────────────────────────
sembrar()
crear(nuevo_shared(), "LG")
ok("4 a plain create: written, \"creada\" notice, log and sync bump",
   n_ini("LG") == 1L && identical(TIPOS, "message") && length(LOGS) == 1L &&
     LOGS[[1]]$action == "crear_empresa" && BUMPS == 1L)

# ── 5. account_code: two creates at once with different initials ────────────
# Both copies end at E0002, so both compute E0003. B's is moved to the next
# free code in S3's table, and everything after the save -- the "creada"
# notice, the log's description and metadata -- names the code written.
sembrar()
sa <- nuevo_shared(); sb <- nuevo_shared()
crear(sa, "NT")
limpiar()
crear(sb, "LG")
e <- leer()
cod_nt <- e$account_code[e$initials == "NT"]; cod_lg <- e$account_code[e$initials == "LG"]
ok("5a both are written, with different account codes (E0003 and E0004)",
   identical(cod_nt, "E0003") && identical(cod_lg, "E0004") && !anyDuplicated(e$account_code))
ok("5b B's notice and log name the code written (E0004), never the one computed on its copy (E0003)",
   identical(AVISOS, es("creada", iniciales = "LG", codigo = "E0004")) && length(LOGS) == 1L &&
     identical(LOGS[[1]]$metadata$account_code, "E0004") && grepl("(E0004)", LOGS[[1]]$description, fixed = TRUE))
ok("5c a free code is kept as computed (A's create: E0003, no gap)", identical(cod_nt, "E0003"))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
