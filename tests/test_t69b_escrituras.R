# =============================================================================
# tests/test_t69b_escrituras.R
# T69b, writes: a session that reads a treasury table as its empty start
# value (the read gate) may not write it either -- a save of its whole copy
# would wipe the table. .s3_write() asks session$userData$hd_escritura_permitida
# (app.R: nivel_escritura_permitida(tier_efectivo(), key)) before anything.
#   1. nivel_escritura_permitida(): Compras no on every treasury file (and the
#      per-user Agenda / opening balances); yes on pasivos_provisions (its own
#      "Crear provisión", always rebased) and on everything else (requisitions,
#      preferences); unknown tier no on treasury; a full tier yes.
#   2. Every treasury reactive's S3 file is listed (no file left ungated).
#   3. In a Compras session, on an in-memory S3 with the REAL persistence
#      layer: the whole-copy saves it could reach (moves, policy catalog,
#      Agenda, manual items, alias map) are refused; its own writes go
#      through -- a provision on an empty base (adds, keeps the rest), its
#      preferences, a requisitions file -- and every treasury file is byte
#      for byte what it was.
#   4. The same writes in a full-tier session and with no session (jobs) go
#      through, as before.
# Run from project root:  Rscript tests/test_t69b_escrituras.R
# Counter-proof: on 13ba188 (read gate only) the Compras saves of 3 go
# through and wipe the tables.
# =============================================================================
cat("=== test_t69b_escrituras ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(a, b) if (is.null(a)) b else a

# ── In-memory S3: the project's helper (swaps aws.s3 and restores it) ─────────
# It sources R/persistence.R with S3_KEYS, on bucket "fake-bucket", client acme.
source("tests/helpers/s3_memoria.R")
store <- .mem
for (f in c("R/tier_registry.R", "R/pasivos_schemas.R", "R/pasivos_persistence.R", "R/avisos.R"))
  suppressMessages(source(f))
q <- function(expr) suppressMessages(expr)

COMPRAS <- "compras"
TOTAL <- names(TIER_REGISTRY)[vapply(TIER_REGISTRY, function(e) is.null(e$pestanas) && !isTRUE(e$interno), logical(1))][1]

# ── 1. Who may write what ────────────────────────────────────────────────────
tes <- c(names(HD_ARCHIVOS_TESORERIA), "pagar_hoy_user_ana.rds", "saldos_apertura_user_ana.rds")
ok("Compras may write no treasury file", !any(vapply(tes, function(k) nivel_escritura_permitida(COMPRAS, k), logical(1))))
ok("Compras may write its provisions, preferences, requisitions and audit",
   all(vapply(c(S3_KEYS$pasivos_provisions, "preferencias.rds", "requisiciones.rds", S3_KEYS$pasivos_audit, S3_KEYS$app_audit),
              function(k) nivel_escritura_permitida(COMPRAS, k), logical(1))))
ok("an unknown tier may write no treasury file (fail closed), and the rest yes",
   !nivel_escritura_permitida(NULL, S3_KEYS$moves) && !nivel_escritura_permitida("", S3_KEYS$pagar_hoy) &&
     nivel_escritura_permitida(NULL, "requisiciones.rds"))
ok("a full tier may write everything", all(vapply(tes, function(k) nivel_escritura_permitida(TOTAL, k), logical(1))))

# ── 2. Every treasury reactive's file is listed ──────────────────────────────
ok("every S3 file named in HD_ARCHIVOS_TESORERIA is a real S3 key",
   all(names(HD_ARCHIVOS_TESORERIA) %in% unlist(S3_KEYS)))
ok("every file maps to a gated reactive", all(HD_ARCHIVOS_TESORERIA %in% names(HD_REACTIVOS_TESORERIA)))
sin_archivo <- setdiff(names(HD_REACTIVOS_TESORERIA), c(HD_ARCHIVOS_TESORERIA, HD_ARCHIVOS_TESORERIA_PATRON,
                                                         "sap_data", "sap_snapshot_info", "pasivos_provisions_db"))
ok(sprintf("every gated reactive saved from the session has its file listed (left: %s)",
           if (length(sin_archivo)) paste(sin_archivo, collapse = ",") else "none"), !length(sin_archivo))

# ── 3. A Compras session on the in-memory S3 ─────────────────────────────────
sembrar <- function() {
  rm(list = ls(store), envir = store)
  for (env in Filter(Negate(is.null), list(get0(".s3_preload_cache"), get0(".s3_missing_cache"), get0(".s3_filas"))))
    rm(list = ls(env), envir = env)
  for (k in c(S3_KEYS$moves, S3_KEYS$policy_catalog, S3_KEYS$pagar_hoy, S3_KEYS$manual, S3_KEYS$parte_alias_map))
    assign(paste0("acme/", k), data.frame(id = as.character(1:5), x = "dato", stringsAsFactors = FALSE), envir = store)
  assign(paste0("acme/", S3_KEYS$pasivos_provisions),
         .normalize(data.frame(id = c("p1", "p2"), estado = "provisional", stringsAsFactors = FALSE), .schema_pasivos_provision),
         envir = store)
}
huella <- function() {
  k <- paste0("acme/", c(S3_KEYS$moves, S3_KEYS$policy_catalog, S3_KEYS$pagar_hoy, S3_KEYS$manual, S3_KEYS$parte_alias_map))
  vapply(k, function(x) digest_raw(get(x, envir = store)), character(1))
}
digest_raw <- function(x) paste(as.character(serialize(x, NULL)), collapse = "")
vacio <- data.frame(id = character(), x = character(), stringsAsFactors = FALSE)

en_sesion <- function(tier, expr) {
  res <- NULL
  testServer(function(input, output, session) {
    nivel <- reactiveVal(tier)
    session$userData$hd_escritura_permitida <- function(key) nivel_escritura_permitida(nivel(), key)
  }, { res <<- eval(expr) })
  res
}
# Refused = an error of class "escritura_no_permitida" (never a silent return).
rechaza <- function(expr) inherits(tryCatch({ q(expr); "escrito" }, error = function(e) e), "escritura_no_permitida")

sembrar(); antes <- huella()
r <- en_sesion(COMPRAS, quote(list(
  moves   = rechaza(save_moves(.schema_moves(), client_id = "acme")),
  catalog = rechaza(.s3_write(vacio, S3_KEYS$policy_catalog, client_id = "acme")),
  agenda  = rechaza(.s3_write(vacio, S3_KEYS$pagar_hoy, client_id = "acme")),
  manual  = rechaza(.s3_write(vacio, S3_KEYS$manual, client_id = "acme")),
  alias   = rechaza(.s3_write(vacio, S3_KEYS$parte_alias_map, client_id = "acme")),
  prov    = tryCatch({ q(save_pasivos_provisions(
              .normalize(data.frame(id = "p-nueva", estado = "provisional", stringsAsFactors = FALSE), .schema_pasivos_provision),
              client_id = "acme", base = .schema_pasivos_provision())); TRUE }, error = function(e) conditionMessage(e)),
  prefs   = tryCatch({ q(prefs_guardar("acme", "ana", list(req_avisos = list(modo = "mias")))); TRUE }, error = function(e) conditionMessage(e)),
  req     = tryCatch({ q(.s3_write(data.frame(id = "r1"), "requisiciones.rds", client_id = "acme")); TRUE }, error = function(e) conditionMessage(e))
)))
ok("Compras: the whole-copy save of moves is refused", isTRUE(r$moves))
ok("Compras: policy catalog, Agenda, manual items and alias map writes are refused",
   isTRUE(r$catalog) && isTRUE(r$agenda) && isTRUE(r$manual) && isTRUE(r$alias))
ok(paste("Compras: a provision on an empty base goes through:", r$prov), isTRUE(r$prov))
provs <- get(paste0("acme/", S3_KEYS$pasivos_provisions), envir = store)
ok("…and it ADDS: the 2 provisions that were there stay", setequal(provs$id, c("p1", "p2", "p-nueva")))
ok(paste("Compras: its preferences are saved:", r$prefs), isTRUE(r$prefs) && exists(paste0("acme/", .PREFERENCIAS_KEY), envir = store))
ok(paste("Compras: a requisitions file is written:", r$req), isTRUE(r$req))
ok("every treasury file is byte for byte what it was", identical(huella(), antes))

ok("the refusal leaves a trail with the table and the tier in the server log",
   any(grepl("[T69b] write refused: table=invoice_moves.rds tier=compras",
             capture.output(en_sesion(COMPRAS, quote({
               session$userData$hd_nivel_efectivo <- function() "compras"
               tryCatch(save_moves(.schema_moves(), client_id = "acme"), error = function(e) NULL) })), type = "message"),
             fixed = TRUE)))

# ── 3b. A real screen: Políticas, saved by a staff "ver como Compras" ────────
# (staff holds can_manage_policies; the effective tier is Compras). The save
# is refused: the screen shows its error and never "Política creada".
for (f in c("R/i18n.R", "R/permisos.R", "R/settings/settings_policies.R")) suppressMessages(source(f))
avisos <- list()
showNotification <- function(ui, ..., type = "default") { avisos[[length(avisos) + 1L]] <<- list(texto = as.character(ui), tipo = type); invisible() }
log_action <- function(...) invisible(NULL)
bump_sync_version <- function(...) invisible(NULL)
sembrar(); antes <- huella()
STAFF <- names(TIER_REGISTRY)[vapply(TIER_REGISTRY, function(e) isTRUE(e$interno), logical(1))][1]
tryCatch(testServer(function(input, output, session) {
  catalogo <- reactiveVal(get(paste0("acme/", S3_KEYS$policy_catalog), envir = store))
  shared <- list(
    usuario = function() list(user = "staff", tier = STAFF, permisos = list(can_manage_policies = TRUE)),
    current_user_info = function() list(username = "staff", is_staff = TRUE, tier = STAFF),
    effective_client_id = function() "acme", home_client_id = function() "hd-admin",
    policy_catalog_db = catalogo, partner_policies_db = reactiveVal(NULL))
  session$userData$hd_escritura_permitida <- function(key) nivel_escritura_permitida(COMPRAS, key)
  session$userData$hd_nivel_efectivo <- function() COMPRAS
  settings_policies_observer(input, output, session, shared)
}, {
  # Under testServer the first value of an ignoreInit observer is not an
  # event: the button goes 1 then 2, as two clicks would.
  session$setInputs(pol_btn_new = 1); session$setInputs(pol_btn_new = 2)
  session$setInputs(pol_name = "Quincena", pol_type = "weekdays", pol_roll_direction = "forward")
  session$setInputs(pol_btn_save = 1)
}), error = function(e) message("  testServer: ", conditionMessage(e)))
tipos <- vapply(avisos, function(a) a$tipo, character(1)); textos <- vapply(avisos, function(a) a$texto, character(1))
ok(sprintf("Políticas, refused save: the screen shows the save error with the refusal (%s)", paste(tipos, textos, collapse = " | ")),
   any(tipos == "error" & grepl("T69b", textos, fixed = TRUE)))
ok("…and never the success notice", !any(grepl(tr("tablas.ajustes.aviso.politica_creada"), textos, fixed = TRUE)))
ok("…and the policy catalog is byte for byte what it was", identical(huella(), antes))
rm(showNotification, log_action, bump_sync_version)

# ── 4. A full tier, and no session at all, as before ─────────────────────────
sembrar()
r <- en_sesion(TOTAL, quote(tryCatch({ q(.s3_write(vacio[0, ], S3_KEYS$policy_catalog, client_id = "acme", permitir_reduccion = TRUE)); TRUE },
                                     error = function(e) conditionMessage(e))))
ok(paste("a full tier writes a treasury file, as before:", r), isTRUE(r))
sembrar()
ok("no session (a job, a script): writes as before",
   isTRUE(tryCatch({ q(.s3_write(vacio[0, ], S3_KEYS$moves, client_id = "acme", permitir_reduccion = TRUE)); TRUE },
                   error = function(e) conditionMessage(e))))

s3_memoria_restaurar()
cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
