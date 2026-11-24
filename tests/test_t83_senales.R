# =============================================================================
# tests/test_t83_senales.R
# T83: an account activated (or changed) in another session did not show in
# Usuarios until a reload, nor did an invitation's change in Invitaciones:
# both lists were read once per session and nothing told them. Now
#   - auth_save_usuarios() sends the sync bus's signal "usuarios_db" for its
#     client once the save worked (every writer of usuarios goes through it);
#   - modificar_pending_invites() and the purge send "invitaciones_db" to
#     hd-admin and to each client whose invitations changed;
#   - app.R registers both as signals, with no table's key (the offline queue
#     stages a registered table's failed write: never usuarios/invitations);
#   - Usuarios re-reads on the signal, not while a permission editor is open
#     (then once it closes); Invitaciones bumps its ↻.
# S3 is never touched: every writer is a stub in the test's own environment.
# Run from project root:  Rscript tests/test_t83_senales.R
# =============================================================================
cat("=== test_t83_senales ===\n\n")
suppressPackageStartupMessages(library(shiny))
`%||%` <- function(a, b) if (is.null(a)) b else a

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
# Top-level assignments of a file, by name, into env.
tomar <- function(archivo, nombres, env) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) && as.character(e[[2]]) %in% nombres)
      eval(e, env)
  env
}
cuerpo <- function(env, f) paste(deparse(get(f, envir = env), width.cutoff = 500L), collapse = "\n")

# ── A. avisar_senal(): one bump per client, only when registered ──────────────
A <- tomar("R/auth.R", c("avisar_senal", "SENAL_USUARIOS", "SENAL_INVITACIONES", "auth_save_usuarios"), new.env())
bumps <- character()
A$bump_sync_version <- function(name, client_id = NULL) { bumps <<- c(bumps, paste(name, client_id)); invisible(TRUE) }
A$.SYNC_REGISTRY <- list(usuarios_db = list(), invitaciones_db = list())
r <- tryCatch(A$avisar_senal("usuarios_db", c("Acme", "acme ", NA, "", "hd-admin")),
              error = function(e) NULL)   # master has no avisar_senal: the checks fail, the test goes on
ok("A1 one bump per distinct client (normalized), none for NA/empty",
   isTRUE(r) && identical(bumps, c("usuarios_db acme", "usuarios_db hd-admin")))
bumps <- character()
ok("A2 a name the app did not register: nothing (no warning, no bump)",
   identical(A$avisar_senal("otra_cosa", "acme"), FALSE) && !length(bumps))
A$bump_sync_version <- function(name, client_id = NULL) stop("S3 down")
ok("A3 a failed bump never stops the caller", identical(suppressMessages(A$avisar_senal("usuarios_db", "acme")), TRUE))
rm("bump_sync_version", envir = A)
ok("A4 outside the app (no bump_sync_version): nothing", identical(A$avisar_senal("usuarios_db", "acme"), FALSE))
ok("A5 the signals' keys are no table's (not in S3_KEYS)", {
  G <- new.env(); tomar("R/global.R", "S3_KEYS", G)
  !any(c(A$SENAL_USUARIOS, A$SENAL_INVITACIONES) %in% unlist(G$S3_KEYS))
})

# ── B. auth_save_usuarios(): the signal after the save, for its client ────────
b <- cuerpo(A, "auth_save_usuarios")
ok("B1 auth_save_usuarios signals usuarios_db for cid",
   grepl('avisar_senal("usuarios_db", cid)', b, fixed = TRUE))
ok("B2 ... only after s3saveRDS (a refused or failed save signals nothing)",
   regexpr("s3saveRDS", b, fixed = TRUE) < regexpr("avisar_senal", b, fixed = TRUE))
ok("B3 every writer of usuarios.rds goes through auth_save_usuarios (no other s3saveRDS of it)", {
  # Code only: a comment that names both (T72-G's, persistence.R) is not a writer.
  codigo <- function(f) sub("#.*$", "", readLines(f, warn = FALSE))
  fs <- c("app.R", list.files("R", pattern = "\\.R$", full.names = TRUE))
  hits <- unlist(lapply(fs, function(f) { x <- codigo(f)
    i <- grep("s3saveRDS", x, fixed = TRUE); if (length(i)) paste(f, i) else character() }))
  ctx <- vapply(hits, function(h) { p <- strsplit(h, " ")[[1]]; x <- codigo(p[1])
    n <- as.integer(p[2]); paste(x[max(1, n - 3):n], collapse = " ") }, "")
  !any(grepl("usuarios", ctx) & !grepl("auth\\.R", names(ctx)))
})

# ── C. .invitaciones_clientes_tocados() ───────────────────────────────────────
P <- tomar("R/persistence.R", c(".invitaciones_clientes_tocados", "modificar_pending_invites",
                                 "purgar_invitaciones_vencidas"), new.env())
inv <- function(tok, cid, st = "pending") data.frame(token = tok, client_id = cid, status = st, stringsAsFactors = FALSE)
antes <- inv(c("t1", "t2", "t3"), c("acme", "beta", "gama"))
ok("C1 nothing changed: no client", !length(P$.invitaciones_clientes_tocados(antes, antes)))
ok("C2 added, removed and changed are each found; the untouched one is not", {
  despues <- inv(c("t1", "t3", "t4"), c("acme", "gama", "delta"), c("pending", "accepted", "pending"))
  setequal(P$.invitaciones_clientes_tocados(antes, despues), c("beta", "gama", "delta"))
})
ok("C3 from an empty table, and to one", {
  vacio <- antes[0, ]
  setequal(P$.invitaciones_clientes_tocados(vacio, antes), c("acme", "beta", "gama")) &&
    setequal(P$.invitaciones_clientes_tocados(antes, vacio), c("acme", "beta", "gama"))
})

# ── D. modificar_pending_invites(): hd-admin and the touched clients ──────────
P$.HD_ADMIN_CID <- "hd-admin"; P$.schema_pending_invites <- function() inv(character(), character())
guardado <- NULL; senales <- list()
P$modificar_hd_admin <- function(key, schema_fn, cambio, evento = NULL) { guardado <<- cambio(antes); guardado }
P$avisar_senal <- function(nombre, clientes) { senales[[length(senales) + 1L]] <<- list(nombre, clientes); invisible(TRUE) }
r <- tryCatch(P$modificar_pending_invites(function(x) { x$status[x$token == "t2"] <- "accepted"; x }),
              error = function(e) NULL)
ok("D1 the change is written as before and its result returned",
   identical(r, guardado) && identical(guardado$status, c("pending", "accepted", "pending")))
ok("D2 one signal: invitaciones_db to hd-admin and the accepted invitation's client",
   length(senales) == 1L && identical(senales[[1]][[1]], "invitaciones_db") &&
     identical(senales[[1]][[2]], c("hd-admin", "beta")))
senales <- list()
P$modificar_hd_admin <- function(key, schema_fn, cambio, evento = NULL) stop("[S3] write failed")
ok("D3 a failed write signals nothing (and still fails)",
   inherits(tryCatch(P$modificar_pending_invites(identity), error = function(e) e), "error") && !length(senales))
ok("D4 the purge signals hd-admin and the clients it deleted from, only when it deleted",
   grepl('avisar_senal("invitaciones_db", c(.HD_ADMIN_CID, names(borradas)))', cuerpo(P, "purgar_invitaciones_vencidas"), fixed = TRUE) &&
     grepl("if (length(borradas)", cuerpo(P, "purgar_invitaciones_vencidas"), fixed = TRUE))

# ── E. app.R: both registered as signals, both in shared ──────────────────────
app <- readLines("app.R", warn = FALSE)
ok("E1 app.R registers usuarios_db and invitaciones_db with the signal keys",
   any(grepl('register_synced("usuarios_db",     SENAL_USUARIOS,', app, fixed = TRUE)) &&
     any(grepl('register_synced("invitaciones_db", SENAL_INVITACIONES,', app, fixed = TRUE)))
ok("E2 shared carries both reactiveVals (the bus pushes into shared[[name]])",
   any(grepl("usuarios_db       = reactiveVal(0)", app, fixed = TRUE)) &&
     any(grepl("invitaciones_db   = reactiveVal(0)", app, fixed = TRUE)))

# ── F. Usuarios and Invitaciones listen (the real lines of tiersServer) ───────
# The statements are taken from tiers_module.R itself and run in a module.
tx <- parse("R/tiers_module.R", keep.source = FALSE)
ts <- NULL
for (e in tx) if (is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("tiersServer"))) ts <- e[[3]]
mod_body <- NULL
buscar <- function(x) {
  if (!is.null(mod_body) || !is.call(x)) return()
  if (identical(x[[1]], as.name("moduleServer"))) { mod_body <<- x[[3]][[3]]; return() }
  for (a in as.list(x)[-1]) if (!identical(a, quote(expr = ))) buscar(a)
}
buscar(ts[[3]])
stmts <- as.list(mod_body)[-1]
txt <- vapply(stmts, function(s) paste(deparse(s, width.cutoff = 500L), collapse = " "), "")
toma <- stmts[grepl("^\\.usuarios_(senal|releer_pendiente|releer_ya) <-", txt) |
              grepl("^observeEvent\\(\\.usuarios_senal\\(\\)", txt) |
              grepl("^observe\\(\\{.*\\.usuarios_releer_pendiente", txt) |
              grepl("^\\.invitaciones_senal <-", txt) |
              grepl("^observeEvent\\(\\.invitaciones_senal\\(\\)", txt)]
ok("F0 the 7 statements are in tiersServer", length(toma) == 7L)
ok("F1 all_usuarios_rv depends on .usuarios_releer()",
   any(grepl("^all_usuarios_rv <- reactive\\(\\{ *\\.usuarios_releer\\(\\)", txt)))

testServer(function(id) moduleServer(id, function(input, output, session) {
  shared <- list(usuarios_db = reactiveVal(0), invitaciones_db = reactiveVal(0))
  .usuarios_local <- reactiveVal("copia local")
  .usuarios_releer <- reactiveVal(0L)
  selected_user <- reactiveVal(NULL)
  invites_refresh <- reactiveVal(0L)
  for (s in toma) eval(s)
  session$userData$t <- environment()
}), {
  t <- session$userData$t
  session$flushReact()
  ok("F2 at start nothing is re-read (ignoreInit)", t$.usuarios_releer() == 0L && identical(t$.usuarios_local(), "copia local"))
  t$shared$usuarios_db(1); session$flushReact()
  ok("F3 the signal drops the local copy and re-reads",
     t$.usuarios_releer() == 1L && is.null(t$.usuarios_local()))
  t$.usuarios_local("copia tras guardar"); t$selected_user("ana")
  t$shared$usuarios_db(2); session$flushReact()
  ok("F4 with a permission editor open: not yet",
     t$.usuarios_releer() == 1L && identical(t$.usuarios_local(), "copia tras guardar"))
  t$selected_user(NULL); session$flushReact()
  ok("F5 once it closes: re-read, once", t$.usuarios_releer() == 2L && is.null(t$.usuarios_local()))
  session$flushReact()
  ok("F6 ... and not again", t$.usuarios_releer() == 2L)
  t$shared$invitaciones_db(1); session$flushReact()
  ok("F7 Invitaciones: the signal bumps its refresh", t$invites_refresh() == 1L)
})

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
