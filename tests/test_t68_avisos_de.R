# =============================================================================
# tests/test_t68_avisos_de.R
# T68 "De quién recibo avisos": the data, the gate and the deliveries.
# In-memory S3 (tests/helpers/s3_memoria.R; aws.s3 swapped and restored).
# The integrator's conditions (requisiciones/bitacora.md, 2026-11-20):
#   1 prefs_reemplazar(): replaces only [[usuario]][[clave]], re-reads, a
#     failed read writes nothing; other users and keys survive; `cambia` of
#     one stays an array;
#   2 the gate is permissions only (Coordinador): an "act" is published when
#     another user of the client holds the action's permission; unreadable
#     users publish all the same, and the delivery still filters by permission;
#   3 crear_requisicion is no longer "act"; an old one still shows;
#   4 req_avisos_destinatarios(): without avisos_de, the same people as
#     before (29d829e) for every T62 mode; with it, avisos_de decides;
#   5 permission by action: Compras never gets the treasury channels, the
#     server trims them on save, and Compras saves with the T69b write gate on;
#   6 HopDesk staff save in THEIR home client (avisos_de_clientes[[client]]),
#     never in the client's folder; no requisitions, no "lo mio".
# Run from the repo root:  Rscript tests/test_t68_avisos_de.R
# =============================================================================
library(testthat)
suppressPackageStartupMessages({ library(shiny) })
source("tests/helpers/s3_memoria.R")
`%||%` <- function(a, b) if (is.null(a)) b else a
.tomar <- function(texto_o_archivo, nombres, env = globalenv()) {
  ex <- if (length(texto_o_archivo) == 1L && file.exists(texto_o_archivo)) parse(texto_o_archivo, keep.source = FALSE)
        else parse(text = texto_o_archivo, keep.source = FALSE)
  for (e in ex) if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% nombres) eval(e, env)
}
for (f in c("R/i18n.R", "R/tier_registry.R", "R/permisos.R", "R/avisos.R", "R/panorama_datos.R")) suppressMessages(source(f))
.tomar("R/avisos_alertas_module.R", c(".ALERTAS_KEY", "ALERTAS_COLUMNAS", "ALERTAS_VENTANA_MIN", "ALERTAS_ACCIONES",
                                      "ALERTAS_RETIRADAS", ".schema_avisos_alertas", ".alertas_cid", "alertas_leer",
                                      "alertas_preparar", "alertas_alguien_puede"))
.tomar("R/requisiciones_persistence.R", "req_avisos_destinatarios")
antes <- new.env()
.tomar(system2("git", c("show", "29d829e:R/requisiciones_persistence.R"), stdout = TRUE), "req_avisos_destinatarios", antes)
CID <- "cliente_t68"

U <- data.frame(username = c("ana", "beto", "caro", "dani", "staff1"),
                tier = c("finance", "admin", "compras", "compras", "hopdesk"),
                account_code = c("A1", "B1", "C1", "D1", "S1"), permisos = "{}", deleted = FALSE, activo = TRUE,
                display_name = c("Ana", "Beto", "Caro", "Dani", "Staff"), puesto = c("tesorera", NA, "COMPRAS ", "compras", NA),
                stringsAsFactors = FALSE)
yo <- function(user) { i <- match(user, U$username); list(user = user, tier = U$tier[i], permisos = permisos_resueltos(U$tier[i])) }
prefs_df <- function(...) {
  x <- list(...)
  if (!length(x)) return(data.frame(usuario = character(), datos = character(), actualizado = as.POSIXct(character()),
                                    stringsAsFactors = FALSE))
  data.frame(usuario = names(x), datos = vapply(x, function(d) as.character(jsonlite::toJSON(d, auto_unbox = TRUE)), character(1)),
             actualizado = Sys.time(), stringsAsFactors = FALSE, row.names = NULL)
}
cod <- avisos_de_codigos(U)

# ── the data ─────────────────────────────────────────────────────────────────
test_that("migration when read: each T62 mode and the matrix", {
  m <- function(modo, personas = character()) avisos_de_migrar(list(modo = modo, personas = personas), NULL, cod)
  expect_false(m("ninguna")$mio); expect_true(m("mias")$mio)
  expect_identical(m("mias")$canales$req, list(base = "nadie", cambia = character()))
  expect_identical(m("mias_y", c("B1", "C1"))$canales$req, list(base = "nadie", cambia = c("B1", "C1")))
  expect_identical(m("todas")$canales$req$base, "todos")
  expect_true(avisos_de_migrar(NULL, NULL, cod)$mio)                                   # never saved: "mias"
  matriz <- data.frame(usuario = c("ana", "beto"), crear = c(TRUE, FALSE), archivo_pago = FALSE,
                       confirmar_pago = c(TRUE, TRUE), bancos = FALSE, stringsAsFactors = FALSE)
  p <- avisos_de_migrar(NULL, matriz, cod)
  expect_identical(p$canales$crear$cambia, "A1")
  expect_identical(p$canales$confirmar_pago$cambia, c("A1", "B1"))
  expect_identical(p$canales$bancos$cambia, character())
  expect_identical(avisos_de_de(list(), matriz, cod)$canales$crear$cambia, "A1")       # no avisos_de: migrated
})

test_that("base + exceptions: 'todos menos dos' and someone new", {
  p <- avisos_de_vacio(); p$canales$bancos <- list(base = "todos", cambia = c("B1", "C1"))
  expect_false(avisos_de_recibe(p, "bancos", "B1"))
  expect_true(avisos_de_recibe(p, "bancos", "A1"))
  expect_true(avisos_de_recibe(p, "bancos", "Z9"))                                     # joined later: the column says Todos
  expect_false(avisos_de_recibe(p, "req", "A1"))
  tab <- data.frame(id = sprintf("P%02d", 1:10), bancos = c(rep(TRUE, 8), FALSE, FALSE), req = c(TRUE, TRUE, rep(FALSE, 8)))
  d <- avisos_de_desde_tabla(tab, c("bancos", "req"), TRUE)
  expect_identical(d$canales$bancos, list(base = "todos", cambia = c("P09", "P10")))
  expect_identical(d$canales$req, list(base = "nadie", cambia = c("P01", "P02")))
})

test_that("normalize: anything stored becomes a clean preference; gone people drop out", {
  expect_identical(avisos_de_normalizar("basura"), avisos_de_vacio())
  p <- avisos_de_normalizar(list(mio = FALSE, canales = list(req = list(base = "todos", cambia = list("A1", "X", "")),
                                                              bancos = list(base = "raro"))), codigos = c("A1", "B1"))
  expect_false(p$mio)
  expect_identical(p$canales$req, list(base = "todos", cambia = "A1"))
  expect_identical(p$canales$bancos$base, "nadie")
})

test_that("channels by permission of each ACTION: Compras never gets the treasury ones", {
  expect_setequal(avisos_de_canales_de(yo("caro")), c("req", "crear"))                # crear: crear_provision only
  expect_setequal(avisos_de_canales_de(yo("ana")), c("crear", "archivo_pago", "confirmar_pago", "bancos"))
  expect_setequal(avisos_de_canales_de(yo("beto")), AVISOS_DE_CANALES)
  p <- avisos_de_vacio(); p$canales$bancos <- list(base = "todos", cambia = character())
  expect_identical(avisos_de_recortar(p, c("req", "crear"))$canales$bancos, list(base = "nadie", cambia = character()))
})

test_that("the grid's rows: people by code, tier and job title (normalized), no staff", {
  p <- avisos_de_vacio(); p$canales$req <- list(base = "nadie", cambia = "B1")
  f <- avisos_de_filas(U, p, c("req", "crear"), yo_code = "C1")
  expect_identical(sort(f$id), c("A1", "B1", "C1", "D1"))
  expect_identical(f$req[f$id == "B1"], TRUE); expect_identical(f$req[f$id == "A1"], FALSE)
  expect_identical(f$puesto[f$id %in% c("C1", "D1")], c("Compras", "Compras"))
  expect_identical(f$puesto[f$id == "B1"], tr("panorama.avisos_de.sin_puesto"))
  expect_true(grepl(tr("panorama.avisos_de.tu"), f$nombre[f$id == "C1"], fixed = TRUE))
})

test_that("3: the feed by preference; an old crear_requisicion 'act' still shows", {
  df <- data.frame(clave = c("act|confirmar_pago|ana", "act|importar_bancos|beto", "act|crear_requisicion|ana", "req:x|ana", NA),
                   titulo = letters[1:5], stringsAsFactors = FALSE)
  p <- avisos_de_vacio(); p$canales$confirmar_pago <- list(base = "nadie", cambia = "A1")
  out <- avisos_de_filtrar_feed(df, p, cod)
  expect_identical(out$titulo, c("a", "c", "d", "e"))
  expect_false("crear_requisicion" %in% names(ALERTAS_ACCIONES))
  expect_null(alertas_preparar(CID, "crear_requisicion", "ana"))                       # never "act" again
})

test_that("2: the gate is permissions only: another user of the client with the action's permission", {
  expect_true(avisos_de_alguien_puede(U, NULL, "importar_bancos", "ana"))      # beto (admin) may see Bancos
  solo <- U[U$username %in% c("ana", "caro", "dani"), ]
  expect_false(avisos_de_alguien_puede(solo, NULL, "importar_bancos", "ana"))  # only buyers besides the author
  expect_true(avisos_de_alguien_puede(solo, NULL, "crear_provision", "ana"))   # buyers may see crear_provision
  expect_false(avisos_de_alguien_puede(U[U$username == "ana", ], NULL, "importar_bancos", "ana"))   # nobody else
  expect_false(avisos_de_alguien_puede(U, NULL, "no_existe", "ana"))
})

# ── S3 ───────────────────────────────────────────────────────────────────────
sembrar <- function(prefs = NULL) {
  s3_memoria_reset()
  .s3_write(U, S3_KEYS$usuarios, client_id = CID)
  .s3_write(list(), S3_KEYS$tiers_config, client_id = CID)
  if (!is.null(prefs)) .s3_write(prefs, .PREFERENCIAS_KEY, client_id = CID)
}
s3_memoria_fallar <- function(key, cid) .mem_falla <<- union(.mem_falla, .s3_key(key, client_id = cid))
s3_memoria_fallar_no <- function(key, cid) .mem_falla <<- setdiff(.mem_falla, .s3_key(key, client_id = cid))
crudo <- function(user) { p <- .s3_read_with(.PREFERENCIAS_KEY, client_id = CID); p$datos[tolower(p$usuario) == user] }

test_that("1: prefs_reemplazar replaces the block whole; the user's other keys and other users survive", {
  sembrar(prefs_df(ana = list(ultima_visita = "2026-11-19", req_avisos = list(modo = "mias_y", personas = list("B1"))),
                   beto = list(idioma = "en")))
  a <- avisos_de_vacio(); a$canales$req <- list(base = "nadie", cambia = c("B1", "C1")); a$canales$bancos$base <- "todos"
  prefs_reemplazar(CID, "ana", "avisos_de", avisos_de_para_guardar(a))
  prefs_guardar(CID, "beto", list(idioma = "es"))                          # another write in between
  b <- avisos_de_vacio(); b$canales$req <- list(base = "nadie", cambia = "D1")
  prefs_reemplazar(CID, "ana", "avisos_de", avisos_de_para_guardar(b))
  leido <- jsonlite::fromJSON(crudo("ana"), simplifyVector = FALSE)
  expect_identical(leido$avisos_de$canales$bancos$base, "nadie")            # replaced, not merged
  expect_identical(leido$ultima_visita, "2026-11-19")
  expect_identical(leido$req_avisos$personas, list("B1"))                  # an array of one stays one
  expect_true(grepl('"cambia":["D1"]', crudo("ana"), fixed = TRUE))       # cambia of one: an array
  expect_identical(jsonlite::fromJSON(crudo("beto"))$idioma, "es")
})

test_that("1: a failed read writes nothing", {
  sembrar(prefs_df(ana = list(ultima_visita = "x")))
  antes_raw <- crudo("ana")
  s3_memoria_fallar(.PREFERENCIAS_KEY, CID)
  expect_error(prefs_reemplazar(CID, "ana", "avisos_de", avisos_de_para_guardar(avisos_de_vacio())))
  s3_memoria_fallar_no(.PREFERENCIAS_KEY, CID)
  expect_identical(crudo("ana"), antes_raw)
})

test_that("2: the gate reads S3; unreadable users publish all the same, with the action's permission", {
  sembrar(prefs_df())
  expect_identical(alertas_preparar(CID, "importar_bancos", "ana")$permiso, "can_manage_bancos")
  .s3_write(U[U$username %in% c("ana", "caro"), ], S3_KEYS$usuarios, client_id = CID, permitir_reduccion = TRUE)
  expect_null(alertas_preparar(CID, "importar_bancos", "ana"))                   # no one else may see it
  s3_memoria_fallar(S3_KEYS$usuarios, CID)
  p <- NULL
  expect_message(p <- alertas_preparar(CID, "importar_bancos", "ana"), "publishing anyway")
  s3_memoria_fallar_no(S3_KEYS$usuarios, CID)
  expect_identical(p$permiso, "can_manage_bancos")
  # the delivery still filters by permission: a Compras reader never sees it
  av <- data.frame(id = "x", tipo = "automatico", titulo = "t", niveles = '["compras","admin"]', usuarios = "[]",
                   permiso = p$permiso, clave = p$clave, vence = as.POSIXct(NA), retirado = FALSE,
                   creado = Sys.time(), stringsAsFactors = FALSE)
  .vigente_orig <- .vigente; .vigente <<- function(df) rep(TRUE, nrow(df))
  on.exit(.vigente <<- .vigente_orig)
  expect_equal(nrow(avisos_visibles(CID, yo("caro"), av)), 0L)
  expect_equal(nrow(avisos_visibles(CID, yo("beto"), av)), 1L)
})

test_that("4: without avisos_de, the same recipients as 29d829e for every T62 mode", {
  for (modo in REQ_AVISOS_MODOS) {
    prefs <- prefs_df(beto = list(req_avisos = list(modo = modo, personas = list("C1"))),
                      caro = list(req_avisos = list(modo = modo)), dani = list(ultima_visita = "x"))
    for (int in list(character(), "beto", c("caro", "dani"))) {
      a <- antes$req_avisos_destinatarios(U, NULL, prefs, int, "ana", c("ana", "caro"))
      d <- req_avisos_destinatarios(U, NULL, prefs, int, "ana", c("ana", "caro"))
      expect_identical(sort(d), sort(a), info = paste(modo, paste(int, collapse = ",")))
    }
  }
})

test_that("4: with avisos_de it decides ('lo mío' + the req column)", {
  prefs <- prefs_df(beto = list(avisos_de = list(v = 1, mio = FALSE, canales = list(req = list(base = "todos", cambia = list("A1"))))),
                    caro = list(avisos_de = list(v = 1, mio = TRUE, canales = list(req = list(base = "nadie", cambia = list())))),
                    dani = list(avisos_de = list(v = 1, mio = FALSE, canales = list(req = list(base = "nadie", cambia = list("A1"))))))
  d <- req_avisos_destinatarios(U, NULL, prefs, interesados = "caro", autor = "ana", de_quien = "ana")
  expect_setequal(d, c("caro", "dani"))   # beto: everyone but A1; caro: hers; dani: A1's
})

test_that("4: an avisos_de saved WITHOUT the req channel: its default is 'nadie' (and 'lo mio' as saved)", {
  prefs <- prefs_df(caro = list(avisos_de = list(v = 1, mio = FALSE, canales = list(bancos = list(base = "todos", cambia = list())))),
                    dani = list(avisos_de = list(v = 1, canales = list())))   # mio not saved: TRUE
  expect_identical(avisos_de_normalizar(list(v = 1, canales = list()))$canales$req, list(base = "nadie", cambia = character()))
  expect_true(avisos_de_normalizar(list(v = 1, canales = list()))$mio)
  d <- req_avisos_destinatarios(U, NULL, prefs, interesados = c("caro", "dani"), autor = "ana", de_quien = "ana")
  expect_identical(d, "dani")             # caro: no req channel and mio FALSE; dani: hers (mio by default)
})

test_that("4: base 'todos' and an author without account_code: everyone gets it (as T62's 'todas')", {
  sin <- U; sin$account_code[sin$username == "ana"] <- NA
  prefs <- prefs_df(caro = list(avisos_de = list(v = 1, mio = FALSE, canales = list(req = list(base = "todos", cambia = list("C1"))))))
  expect_identical(req_avisos_destinatarios(sin, NULL, prefs, interesados = character(), autor = "ana", de_quien = "ana"), "caro")
})

test_that("5: Compras saves its avisos_de with the T69b write gate on; the treasury channels are trimmed", {
  sembrar(prefs_df(caro = list(ultima_visita = "x")))
  estado <- data.frame(id = c("A1", "B1"), req = c(TRUE, FALSE), crear = FALSE, bancos = TRUE, confirmar_pago = TRUE)
  testServer(function(input, output, session) {
    session$userData$hd_escritura_permitida <- function(key) nivel_escritura_permitida("compras", key)
    session$userData$p <- avisos_de_guardar(CID, yo("caro"), estado, TRUE)
  }, {})
  leido <- avisos_de_normalizar(jsonlite::fromJSON(crudo("caro"), simplifyVector = TRUE)$avisos_de)
  expect_identical(leido$canales$req, list(base = "nadie", cambia = "A1"))
  expect_identical(leido$canales$bancos, list(base = "nadie", cambia = character()))      # injected, trimmed
  expect_identical(leido$canales$confirmar_pago, list(base = "nadie", cambia = character()))
  expect_true(nivel_escritura_permitida("compras", .PREFERENCIAS_KEY))                    # preferencias.rds is not treasury
})

test_that("6: staff save in their home client, per client; the client's folder is never written", {
  sembrar(prefs_df(caro = list(ultima_visita = "x")))
  .s3_write(prefs_df(mouse = list(idioma = "es", avisos_de_clientes = list(otro = list(v = 1, mio = FALSE)))),
            .PREFERENCIAS_KEY, client_id = "hopdesk")
  antes_cliente <- .s3_read_with(.PREFERENCIAS_KEY, client_id = CID)
  staff <- list(user = "mouse", tier = "admin", permisos = permisos_resueltos("admin"))   # the jump's tier
  estado <- data.frame(id = c("A1", "B1"), req = TRUE, crear = FALSE, bancos = c(TRUE, FALSE))
  avisos_de_guardar(CID, staff, estado, TRUE, casa = "hopdesk")
  expect_identical(.s3_read_with(.PREFERENCIAS_KEY, client_id = CID), antes_cliente)          # untouched
  casa <- .s3_read_with(.PREFERENCIAS_KEY, client_id = "hopdesk")
  d <- jsonlite::fromJSON(casa$datos[casa$usuario == "mouse"], simplifyVector = TRUE)
  expect_identical(d$idioma, "es")
  expect_true(is.list(d$avisos_de_clientes$otro))                                               # another client's block stays
  p <- avisos_de_staff_de(d, CID)
  expect_identical(p$canales$bancos, list(base = "nadie", cambia = "A1"))
  expect_identical(p$canales$req, list(base = "nadie", cambia = character()))                   # no requisitions
  expect_false(p$mio)
  todo <- avisos_de_staff_de(list(), CID)                                                       # never saved: all activity
  expect_identical(todo$canales$bancos$base, "todos"); expect_identical(todo$canales$req$base, "nadie")
})

s3_memoria_restaurar()
