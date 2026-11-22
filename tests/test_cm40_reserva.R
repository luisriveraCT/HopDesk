# =============================================================================
# tests/test_cm40_reserva.R
# CM40, second commit. A rename OLD -> NEW checked empresas.rds once, ran a
# ten-table cascade, and only then wrote empresas.rds: two sessions inside
# that window both passed. Now NEW is reserved on the company's row first
# (renombre_pendiente, modificar_tabla on S3's table), the cascade runs only
# on a reserved rename, and the confirmation clears it. With S1's rename in
# progress (reserved, cascade not done):
#   A  S2 renames ANOTHER company to the same NEW: refused before any table
#      is touched (iniciales_reservadas); S1 then finishes: one NT, the
#      other company's rows and interco config intact;
#   B  S2 renames the SAME company to other initials: refused
#      (renombre_en_curso); S1 finishes: every table under S1's NEW;
#   C  S2 creates a company with S1's NEW: refused (iniciales_reservadas);
#   D  two reservations of the same NEW, directly: the second is refused.
# And:
#   E  an empresas.rds without the column reads it as NA (no migration), and
#      a reservation works on it;
#   F  the reservation never shows in company_map or the company identity;
#   G  a rename through the panel: reservar_renombre, renombrar_iniciales,
#      confirmar_renombre and editar_empresa logged, the reservation cleared;
#   H  a cascade step that fails: the reservation stays, the table shows the
#      "renombre pendiente" badge, Reintentar finishes it;
#   I  the confirmation write that fails: the reservation stays; Reintentar
#      finishes it.
# The real empresasServer and rename_empresa_initials() over the in-memory S3.
# Against 29d829e, A, B, C, D, E (reservation), G, H and I fail.
# Run from project root:  Rscript tests/test_cm40_reserva.R
# =============================================================================
cat("=== test_cm40_reserva ===\n\n")
source("tests/helpers/s3_memoria.R")
suppressPackageStartupMessages({ library(shiny) })
for (f in c("R/bancos_persistence.R", "R/pasivos_schemas.R", "R/pasivos_persistence.R",
            "R/settings/settings_helpers.R", "R/requisiciones_persistence.R"))
  suppressMessages(source(f))
for (f in c("R/i18n.R", "R/hd_registro.R", "R/hd_piezas.R", "R/monedas.R", "R/hd_columnas.R", "R/hd_table.R",
            "R/empresas_identidad.R", "R/empresas_module.R"))
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
bump_sync_version <- function(...) invisible(NULL)
LOGS <- character(); AVISOS <- character()
log_action <- function(..., action = NULL) { if (identical(action, "sin_evento")) return(invisible(NULL)); LOGS <<- c(LOGS, action %||% "?"); invisible(NULL) }   # T72 B: sin_evento is the write layer's (test_t72b_evento)
showNotification <- function(ui, ..., type = "default") { AVISOS <<- c(AVISOS, as.character(ui)); invisible(NULL) }
limpiar <- function() { LOGS <<- character(); AVISOS <<- character() }

CID <- "a"
k <- function(key) paste0(CID, "/", key)
poner <- function(key, df) assign(k(key), df, envir = .mem)
leer  <- function(key) get(k(key), envir = .mem)
fila  <- function(schema, ...) .normalize(tibble::as_tibble(list(...)), schema)
sembrar <- function() {
  s3_memoria_reset(); limpiar()
  poner(S3_KEYS$empresas, fila(.schema_empresas, id = c("x1", "y1"), account_code = c("E0001", "E0002"),
                               initials = c("AA", "AB"), razon_social = c("AA SA", "AB SA"),
                               nombre_corto = c("AA", "AB"), nombres_alt = "[]", rfc = c("RFCAA", "RFCAB"),
                               activa = TRUE, deleted = FALSE))
  poner(S3_KEYS$ctas_cuentas, fila(.schema_ctas_cuentas, id = c("c1", "c2"), Empresa = c("AA", "AB")))
  poner(S3_KEYS$interco_v2, list(ar_prefix = "C", ap_prefix = "P", rfcs = character(),
                                 companies = list(AA = list(ar = "C-AA"), AB = list(ar = "C-AB"))))
}
emp   <- function() leer(S3_KEYS$empresas)
fila_de <- function(id) { e <- emp(); e[e$id == id, , drop = FALSE] }
ctas  <- function() leer(S3_KEYS$ctas_cuentas)$Empresa
pend  <- function(id) { p <- fila_de(id)$renombre_pendiente; if (is.null(p)) NA_character_ else p }
es    <- function(k, ...) tr(paste0("tablas.empresas.aviso.", k), ..., idioma = "es")
NUEVO <- exists("reservar_renombre_empresa", mode = "function")   # FALSE on the base

# S1's rename in progress: reserved (on the base there is nothing to write
# yet: the cascade has passed its check). Then S1 finishes it.
s1_reserva <- function(id, old, new) if (NUEVO) suppressMessages(reservar_renombre_empresa(id, old, new, CID))
s1_termina <- function(id, old, new) {
  tryCatch(suppressMessages(rename_empresa_initials(old, new, client_id = CID)),
           error = function(e) message("  S1's cascade: ", conditionMessage(e)))
  if (NUEVO) suppressMessages(confirmar_renombre_empresa(id, old, new, CID))
  else { e <- emp(); e$initials[e$id == id] <- new; poner(S3_KEYS$empresas, e) }
}
nuevo_shared <- function() {
  db <- reactiveVal(emp())
  list(empresas_db = db, company_map = reactive(.company_map_from_empresas(db())),
       effective_client_id = function() CID, home_client_id = function() CID,
       current_user = function() "ana", usuario = function() "ana",
       current_user_info = reactive(list(tier = "admin", is_staff = FALSE)),
       jump_client_id = function() NULL, visible_initials = function() NULL,
       group_config = reactiveVal(NULL), sap_data = reactiveVal(NULL))
}
# Guardar on `eid`'s edit panel, drawn with initials `pagina`, typing `nuevas`.
# (Not `id`: inside testServer that is the module's own id.)
editar <- function(eid, pagina, nuevas) testServer(empresasServer, args = list(shared = nuevo_shared()), {
  selected_id(eid); session$flushReact()
  f <- fila_de(eid)
  session$setInputs(edit_ini_apertura = pagina, edit_initials = nuevas, edit_razon_social = f$razon_social,
                    edit_nombre_corto = f$nombre_corto, edit_rfc = f$rfc, edit_activa = TRUE, edit_nombres_alt = "")
  session$setInputs(btn_save_edit = as.numeric(Sys.time()))
})
reintentar <- function(eid) testServer(empresasServer, args = list(shared = nuevo_shared()), {
  selected_id(eid); session$flushReact()
  session$setInputs(btn_reintentar_renombre = as.numeric(Sys.time()))
})

# ── A. Another company to the same NEW ──────────────────────────────────────
sembrar()
s1_reserva("x1", "AA", "NT")
editar("y1", "AB", "NT")
ok("A1 S2's rename of AB -> NT is refused before any table: AB's rows and interco config untouched",
   identical(ctas(), c("AA", "AB")) && identical(leer(S3_KEYS$interco_v2)$companies$AB$ar, "C-AB") &&
     identical(fila_de("y1")$initials, "AB"))
ok("A2 S2 is told NT is held by a rename in progress", identical(AVISOS, es("iniciales_reservadas", iniciales = "NT")))
s1_termina("x1", "AA", "NT")
ok("A3 S1 finishes: one NT (X), AB intact, X's interco config under NT",
   sum(emp()$initials == "NT") == 1L && identical(ctas(), c("NT", "AB")) &&
     identical(leer(S3_KEYS$interco_v2)$companies$NT$ar, "C-AA") && is.na(pend("x1")))

# ── B. The same company to other initials ───────────────────────────────────
sembrar()
s1_reserva("x1", "AA", "NT")
editar("x1", "AA", "NS")
ok("B1 S2's rename AA -> NS is refused (renombre_en_curso): nothing renamed",
   identical(ctas(), c("AA", "AB")) && identical(AVISOS, es("renombre_en_curso", iniciales = "NT")))
s1_termina("x1", "AA", "NT")
ok("B2 S1 finishes: every table and empresas.rds under NT, none under NS",
   identical(ctas(), c("NT", "AB")) && identical(fila_de("x1")$initials, "NT") && !"NS" %in% emp()$initials)

# ── C. A new company with the NEW of a rename in progress ───────────────────
sembrar()
s1_reserva("x1", "AA", "NT")
testServer(empresasServer, args = list(shared = nuevo_shared()), {
  session$setInputs(new_initials = "NT", new_razon_social = "NT SA", new_nombre_corto = "", new_rfc = "",
                    new_nombres_alt = "", new_activa = TRUE)
  session$setInputs(btn_save_new_empresa = 1)
})
ok("C the create of NT is refused while X's rename to NT is in progress",
   !"NT" %in% emp()$initials && identical(AVISOS, es("iniciales_reservadas", iniciales = "NT")))

# ── D. Two reservations of the same NEW ─────────────────────────────────────
sembrar()
r <- if (NUEVO) {
  suppressMessages(reservar_renombre_empresa("x1", "AA", "NT", CID))
  tryCatch({ suppressMessages(reservar_renombre_empresa("y1", "AB", "NT", CID)); NULL }, error = function(e) e)
}
ok("D the second reservation of NT is refused (empresa_iniciales_reservadas); X keeps its own",
   inherits(r, "empresa_iniciales_reservadas") && identical(pend("x1"), "NT") && is.na(pend("y1")))

# ── E. An empresas.rds without the column ───────────────────────────────────
sembrar()
viejo <- emp(); viejo$renombre_pendiente <- NULL; poner(S3_KEYS$empresas, viejo)
ok("E1 read without the column: renombre_pendiente is NA on every row (load_empresas and the strict read)",
   "renombre_pendiente" %in% names(load_empresas(client_id = CID)) &&
     all(is.na(load_empresas(client_id = CID)$renombre_pendiente)) &&
     all(is.na(leer_tabla_para_escribir(S3_KEYS$empresas, CID, .schema_empresas)$renombre_pendiente)))
if (NUEVO) suppressMessages(reservar_renombre_empresa("x1", "AA", "NT", CID))
ok("E2 a reservation works on such a file", identical(pend("x1"), "NT") && is.na(pend("y1")))

# ── F. The reservation stays out of company_map and the identity ────────────
sembrar()
antes_mapa <- .company_map_from_empresas(emp()); antes_id <- empresas_identidad(emp())
if (NUEVO) suppressMessages(reservar_renombre_empresa("x1", "AA", "NT", CID))
ok("F company_map and empresas_identidad() are the same with X's reservation",
   identical(.company_map_from_empresas(emp()), antes_mapa) && identical(empresas_identidad(emp()), antes_id) &&
     !"NT" %in% names(.company_map_from_empresas(emp())))

# ── G. A rename through the panel ───────────────────────────────────────────
sembrar()
editar("x1", "AA", "NT")
ok("G1 renamed: tables and empresas.rds under NT, earlier [AA], the reservation cleared",
   identical(ctas(), c("NT", "AB")) && identical(fila_de("x1")$initials, "NT") && is.na(pend("x1")) &&
     identical(unlist(.iniciales_anteriores_de(fila_de("x1")$iniciales_anteriores)), "AA"))
ok("G2 every step logged, in order (T72)",
   identical(LOGS, c("reservar_renombre", "renombrar_iniciales", "confirmar_renombre", "editar_empresa")))

# ── H. A cascade step that fails, then Reintentar ───────────────────────────
sembrar()
.mem_falla <- k(S3_KEYS$ctas_cuentas)
editar("x1", "AA", "NT")
.mem_falla <- character()
estado <- NULL
testServer(empresasServer, args = list(shared = nuevo_shared()), {
  t <- empresas_tabla(); estado <<- t$estado[t$id == "x1"]
})
ok("H1 incomplete: X keeps AA with its reservation NT, and the table shows the badge",
   identical(fila_de("x1")$initials, "AA") && identical(pend("x1"), "NT") && identical(estado, "renombre_pendiente") &&
     !"confirmar_renombre" %in% LOGS)
limpiar()
reintentar("x1")
ok("H2 Reintentar finishes the same rename: NT everywhere, reservation cleared, confirmation logged",
   identical(ctas(), c("NT", "AB")) && identical(fila_de("x1")$initials, "NT") && is.na(pend("x1")) &&
     "confirmar_renombre" %in% LOGS)

# ── I. The confirmation write fails, then Reintentar ────────────────────────
sembrar()
s1_reserva("x1", "AA", "NT")
.mem_falla_escritura <- k(S3_KEYS$empresas)
reintentar("x1")
.mem_falla_escritura <- character()
ok("I1 the cascade ran but the confirmation failed: X still AA, its reservation NT kept",
   identical(ctas(), c("NT", "AB")) && identical(fila_de("x1")$initials, "AA") && identical(pend("x1"), "NT") &&
     es("cambios_no_guardados") %in% AVISOS)
limpiar()
reintentar("x1")
ok("I2 Reintentar again: X is NT, the reservation cleared",
   identical(fila_de("x1")$initials, "NT") && is.na(pend("x1")) && "confirmar_renombre" %in% LOGS)

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
