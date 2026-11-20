# T61: folders inside folders (padre_id, up to REQ_CARPETA_MAX_NIVEL levels),
# tied to companies (empresas: JSON of company ids, NA = every company),
# and General, the virtual folder (carpeta_id NA, never written).
# The in-memory S3 is the one from tests/test_requisiciones_datos.R (aws.s3
# is never patched). Run from the repo root:
#   Rscript tests/test_requisiciones_carpetas_arbol.R
library(testthat)
suppressPackageStartupMessages({ library(shiny); library(dplyr); library(tibble) })
`%||%` <- function(a, b) if (is.null(a)) b else a

# Only these definitions from files this test does not own.
.tomar <- function(archivo, nombres) {
  for (e in parse(archivo, keep.source = FALSE))
    if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% nombres)
      eval(e, globalenv())
}
.tomar("R/global.R", "S3_KEYS")
.tomar("R/persistence.R", c(".normalize", "leer_tabla_para_escribir", ".s3_leer_para_escribir",
                            "modificar_tabla", "rebase_por_id", "confirmaciones_vigentes", ".schema_manual",
                            ".schema_empresas"))
.tomar("R/bancos_persistence.R", ".schema_bancos_confirmados")
.tomar("R/pasivos_persistence.R", c("etapa_de_provision_en", "PROVISION_PAGADA"))
source("R/pasivos_schemas.R")
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
source("R/avisos.R")
source("R/adjuntos_s3.R")
source("R/empresas_identidad.R")
source("R/requisiciones_persistence.R")

# ── In-memory S3 ─────────────────────────────────────────────────────────────
.mem  <- new.env()     # tables, by full key
.objs <- new.env()     # raw objects (attachments, pasted tables), by full key
.s3_preload_cache <- new.env(); .s3_missing_cache <- new.env()
.fallar_lecturas <- FALSE
.fallar_escrituras <- 0L
.escrituras <- 0L
.s3_key <- function(key, client_id = NULL) {
  if (is.null(client_id) || !nzchar(client_id)) stop("sin client_id en .s3_key")
  paste0(tolower(client_id), "/", key)
}
.s3_read_with <- function(key, client_id = NULL) {
  fk <- .s3_key(key, client_id)
  if (exists(fk, envir = .s3_preload_cache, inherits = FALSE)) return(.s3_preload_cache[[fk]])
  if (.fallar_lecturas) return(NULL)
  .mem[[fk]]
}
.s3_existe <- function(key, client_id) {
  if (.fallar_lecturas) return(NA)
  exists(.s3_key(key, client_id), envir = .mem, inherits = FALSE)
}
.s3_write <- function(obj, key, client_id = NULL, permitir_reduccion = FALSE) {
  if (.fallar_escrituras > 0L) { .fallar_escrituras <<- .fallar_escrituras - 1L; stop("escritura simulada falló") }
  .escrituras <<- .escrituras + 1L
  assign(.s3_key(key, client_id), obj, envir = .mem)
}
.adj_s3_poner <- function(bytes, llave, mime) assign(llave, bytes, envir = .objs)
.adj_s3_traer <- function(llave) {
  if (!exists(llave, envir = .objs, inherits = FALSE)) stop("no existe ", llave)
  get(llave, envir = .objs)
}
reiniciar <- function() {
  rm(list = ls(.mem), envir = .mem); rm(list = ls(.objs), envir = .objs)
  rm(list = ls(.s3_preload_cache), envir = .s3_preload_cache)
  .fallar_lecturas <<- FALSE; .fallar_escrituras <<- 0L
}
tabla <- function(clave, cid = CID) .mem[[paste0(cid, "/", REQ_KEYS[[clave]])]]
foto <- function() as.list(.mem)

CID <- "cliente_prueba"
base_req <- list(titulo = "Cemento para la bodega 3", empresa = "NCS", proveedor = "Cemex",
                 monto = 15000, moneda = "MXN", notas = "Urgente porque la obra empieza el lunes")

poner_provision <- function(id, estado = "provisional", monto = 20000, cid = CID) {
  k <- paste0(cid, "/", S3_KEYS$pasivos_provisions)
  p <- .normalize(.mem[[k]], .schema_pasivos_provision)
  p <- dplyr::bind_rows(p[p$id != id, ], tibble::tibble(id = id, estado = estado, amount_pago = monto,
                                                         empresa = "NCS", parte = "CFE",
                                                         moneda_pago = "MXN", fecha_efectiva = as.Date("2026-12-15")))
  assign(k, p, envir = .mem)
}
poner_confirmacion <- function(provision_id, importe = 19500, eliminado = FALSE, action = "confirmed", cid = CID) {
  k <- paste0(cid, "/", S3_KEYS$bancos_confirmados)
  c <- .normalize(.mem[[k]], .schema_bancos_confirmados)
  c <- dplyr::bind_rows(c, tibble::tibble(confirmacion_id = uuid::UUIDgenerate(), provision_id = provision_id,
                                          importe = importe, moneda = "MXN", eliminado = eliminado,
                                          action = action, confirmado_at = Sys.time()))
  assign(k, c, envir = .mem)
}
archivo_temp <- function(bytes, ext = "bin") { f <- tempfile(fileext = paste0(".", ext)); writeBin(bytes, f); f }
png_real <- function(w = 40, h = 20) magick::image_write(magick::image_blank(w, h, "steelblue"), format = "png")

# ═════════════════════════════════════════════════════════════════════════════
# T61: folders inside folders, and folders tied to companies.
# ═════════════════════════════════════════════════════════════════════════════
crear <- function(nombre, padre = NA, ...) requisiciones_carpeta_crear(CID, list(nombre = nombre, padre_id = padre, ...), "dani")
err <- function(expr) tryCatch({ expr; NULL }, req_error = function(e) e$clave)
fila <- function(id) { a <- requisiciones_carpetas_arbol(tabla("carpetas")); a[a$id == id, ] }
# A table built by hand, never through the write functions.
a_mano <- function(id, padre = NA, nombre = id, empresas = NA, papelera = NA, creado = NULL)
  .normalize(tibble::tibble(id = id, padre_id = padre, nombre = nombre, empresas = empresas,
                            creado = creado %||% (as.POSIXct("2026-11-01", tz = "UTC") + seq_along(id)),
                            papelera_en = as.POSIXct(papelera, tz = "UTC")), .schema_req_carpetas)

test_that("crear en la raíz y dentro: nivel y ruta; destino inexistente o en papelera, rechazado", {
  reiniciar()
  a <- crear("Obra A"); b <- crear("Material", a); c <- crear("Cemento", b)
  expect_identical(fila(c)$ruta, "Obra A › Material › Cemento")
  expect_identical(fila(c)$nivel, 3L); expect_identical(fila(b)$padre_ef, a)
  expect_true(is.na(fila(a)$padre_id)); expect_true(is.na(fila(a)$empresas))   # every company
  expect_identical(requisiciones_carpetas(CID)$id, c(a, b, c))                  # tree order
  antes <- tabla("carpetas")
  expect_identical(err(crear("X", "no-existe")), "requisiciones.error.carpeta_padre")
  requisiciones_carpeta_papelera(CID, c, "d")
  expect_identical(err(crear("X", c)), "requisiciones.error.carpeta_padre")
  expect_identical(nrow(tabla("carpetas")), nrow(antes))
})

test_that("hasta 5 niveles: el sexto se rechaza, al crear y al mover una rama", {
  reiniciar()
  ids <- character(); p <- NA
  for (k in 1:5) { p <- crear(paste("N", k), p); ids <- c(ids, p) }
  expect_identical(fila(ids[5])$nivel, 5L)
  antes <- tabla("carpetas")
  expect_identical(err(crear("N6", ids[5])), "requisiciones.error.carpeta_nivel")
  expect_identical(tabla("carpetas"), antes)
  # A two-level branch fits under level 3, not under level 4.
  r <- crear("R"); crear("R hija", r)
  expect_null(err(requisiciones_carpeta_editar(CID, r, list(padre_id = ids[3]), "dani")))
  expect_identical(err(requisiciones_carpeta_editar(CID, r, list(padre_id = ids[4]), "dani")),
                   "requisiciones.error.carpeta_nivel")
})

test_that("mover: lo de adentro va con ella, sin tocar requisiciones; ciclo rechazado sobre la tabla fresca", {
  reiniciar()
  a <- crear("A"); b <- crear("B"); h <- crear("Hija de A", a)
  r <- requisicion_crear(CID, modifyList(base_req, list(carpeta_id = h)), "dani")
  reqs_antes <- tabla("requisiciones")
  requisiciones_carpeta_editar(CID, a, list(padre_id = b), "eli")
  expect_identical(fila(h)$ruta, "B › A › Hija de A")
  expect_identical(fila(a)$movido_por, "eli"); expect_false(is.na(fila(a)$movido))
  expect_identical(tabla("requisiciones"), reqs_antes)
  expect_identical(err(requisiciones_carpeta_editar(CID, a, list(padre_id = a), "d")), "requisiciones.error.carpeta_ciclo")
  expect_identical(err(requisiciones_carpeta_editar(CID, b, list(padre_id = h), "d")), "requisiciones.error.carpeta_ciclo")
  # Another session put C under D in S3; this screen still sees both at the
  # top and asks to put D under C. The check runs on the fresh table.
  c <- crear("C"); d <- crear("D")
  k <- paste0(CID, "/", REQ_KEYS$carpetas); t <- .mem[[k]]; t$padre_id[t$id == c] <- d; assign(k, t, envir = .mem)
  expect_identical(err(requisiciones_carpeta_editar(CID, d, list(padre_id = c), "d")), "requisiciones.error.carpeta_ciclo")
  # Back to the top.
  requisiciones_carpeta_editar(CID, a, list(padre_id = NA), "eli")
  expect_true(is.na(fila(a)$padre_ef))
})

test_that("nombre único entre hermanas, no entre primas ni entre niveles", {
  reiniciar()
  a <- crear("Obra A"); b <- crear("Obra B")
  m <- crear("Material", a)
  expect_identical(err(crear("MATERIAL", a)), "requisiciones.error.carpeta_repetida")
  expect_null(err(crear("Material", b)))           # a cousin
  expect_null(err(crear("Material")))              # another level
  m2 <- crear("Mano de obra", a)
  expect_identical(err(requisiciones_carpeta_editar(CID, m2, list(nombre = "material"), "d")),
                   "requisiciones.error.carpeta_repetida")
  # Moving it next to a namesake is refused too.
  mb <- crear("Herramienta", b); crear("Herramienta", a)
  expect_identical(err(requisiciones_carpeta_editar(CID, mb, list(padre_id = a), "d")),
                   "requisiciones.error.carpeta_repetida")
})

test_that("papelera: con requisiciones vivas en una nieta se rechaza y dice cuántas; sin ellas se marca solo la raíz", {
  reiniciar()
  a <- crear("A"); b <- crear("B", a); c <- crear("C", b)
  r1 <- requisicion_crear(CID, modifyList(base_req, list(carpeta_id = c)), "dani")
  r2 <- requisicion_crear(CID, modifyList(base_req, list(carpeta_id = c)), "dani")
  antes <- tabla("carpetas")
  e <- tryCatch(requisiciones_carpeta_papelera(CID, a, "d"), req_error = identity)
  expect_identical(e$clave, "requisiciones.error.carpeta_con_requisiciones")
  expect_match(conditionMessage(e), "2")
  expect_identical(tabla("carpetas"), antes)
  requisiciones_mover(CID, c(r1$id, r2$id), NA, "dani")         # to General
  requisiciones_carpeta_papelera(CID, a, "d")
  t <- tabla("carpetas")
  expect_false(is.na(t$papelera_en[t$id == a]))
  expect_true(all(is.na(t$papelera_en[t$id %in% c(b, c)])))     # not marked one by one
  expect_identical(nrow(requisiciones_carpetas(CID)), 0L)        # hidden with it
  expect_identical(err(requisicion_crear(CID, modifyList(base_req, list(carpeta_id = c)), "d")),
                   "requisiciones.error.carpeta")
  # Back, whole and untouched.
  requisiciones_carpeta_restaurar(CID, a, "d")
  expect_identical(requisiciones_carpetas(CID)$id, c(a, b, c))
  t2 <- tabla("carpetas")
  expect_identical(t2[t2$id %in% c(b, c), ], t[t$id %in% c(b, c), ])
})

test_that("restaurar con la madre en papelera: va al ancestro vivo más cercano, en la misma escritura, con lo suyo", {
  reiniciar()
  a <- crear("A"); b <- crear("B", a); c <- crear("C", b); d <- crear("D", c)
  requisiciones_carpeta_papelera(CID, c, "d"); requisiciones_carpeta_papelera(CID, b, "d")
  d_antes <- tabla("carpetas")[tabla("carpetas")$id == d, ]
  n0 <- .escrituras
  requisiciones_carpeta_restaurar(CID, c, "eli")
  expect_identical(.escrituras, n0 + 1L)
  expect_identical(fila(c)$padre_ef, a); expect_identical(fila(c)$movido_por, "eli")
  expect_identical(fila(d)$ruta, "A › C › D")
  expect_identical(tabla("carpetas")[tabla("carpetas")$id == d, ], d_antes)
  expect_false(b %in% requisiciones_carpetas(CID)$id)
  # Everything up the chain in the trash: to the top.
  requisiciones_carpeta_papelera(CID, c, "d"); requisiciones_carpeta_papelera(CID, a, "d")
  requisiciones_carpeta_restaurar(CID, c, "eli")
  expect_true(is.na(fila(c)$padre_ef))
  # A live namesake where it lands: refused, nothing written.
  requisiciones_carpeta_papelera(CID, c, "d"); crear("C")
  antes <- tabla("carpetas")
  expect_identical(err(requisiciones_carpeta_restaurar(CID, c, "d")), "requisiciones.error.carpeta_repetida")
  expect_identical(tabla("carpetas"), antes)
})

test_that("archivo de antes de T61 (sin padre_id ni empresas): raíz y todas las empresas, cero escrituras", {
  reiniciar()
  vieja <- tibble::tibble(id = c("x", "y"), nombre = c("Obra", "Oficina"), color = "#607d8b",
                          presupuesto = NA_real_, moneda = NA_character_, creado_por = "ana",
                          creado = as.POSIXct(c("2026-11-01", "2026-11-02"), tz = "UTC"),
                          papelera_por = NA_character_, papelera_en = as.POSIXct(NA, tz = "UTC"))
  assign(paste0(CID, "/", REQ_KEYS$carpetas), vieja, envir = .mem)
  n0 <- .escrituras
  cs <- requisiciones_carpetas(CID)
  expect_identical(cs$id, c("x", "y"))
  expect_true(all(is.na(cs$padre_ef))); expect_true(all(cs$nivel == 1L)); expect_true(all(is.na(cs$empresas)))
  expect_true(all(requisiciones_carpetas_visibles(cs, activas = "e1", existentes = "e1")))
  v <- requisiciones_vista(CID)
  expect_identical(.escrituras, n0)
  # The first real write keeps both rows.
  crear("Nueva", "x")
  expect_identical(nrow(tabla("carpetas")), 3L)
})

test_that("árbol roto armado a mano (ciclo, padre huérfano, más de 5 niveles): todo termina y nada se pierde", {
  # Cycle A -> B -> A: the one created first goes to the top.
  t <- a_mano(c("A", "B", "H"), padre = c("B", "A", "B"))
  a <- requisiciones_carpetas_arbol(t)
  expect_setequal(a$id, c("A", "B", "H"))
  expect_true(is.na(a$padre_ef[a$id == "A"])); expect_identical(a$padre_ef[a$id == "B"], "A")
  expect_identical(a$ruta[a$id == "H"], "A › B › H")
  # Three-way cycle and a self-loop.
  a <- requisiciones_carpetas_arbol(a_mano(c("P", "Q", "R", "S"), padre = c("R", "P", "Q", "S")))
  expect_identical(nrow(a), 4L); expect_identical(sum(is.na(a$padre_ef)), 2L)
  # Orphan parent: top level.
  a <- requisiciones_carpetas_arbol(a_mano(c("A", "B"), padre = c("no-existe", "A")))
  expect_true(is.na(a$padre_ef[a$id == "A"])); expect_identical(a$nivel[a$id == "B"], 2L)
  # A chain of 8: nobody deeper than 5, nobody lost.
  ids <- paste0("n", 1:8)
  a <- requisiciones_carpetas_arbol(a_mano(ids, padre = c(NA, ids[1:7])))
  expect_identical(nrow(a), 8L); expect_true(all(a$nivel <= REQ_CARPETA_MAX_NIVEL))
  expect_true(is.na(a$padre_ef[a$id == "n6"]))
  # Every walk on it ends.
  expect_setequal(.req_carpeta_subarbol(a, "n1"), ids[1:5])
  expect_length(requisiciones_carpetas_visibles(a, activas = character()), 8L)
  expect_identical(nrow(requisiciones_carpetas_resumen(a, requisiciones_vista_en(
    reqs = .schema_requisiciones(), provs = .schema_pasivos_provision(), conf = .schema_bancos_confirmados(),
    manual = .schema_manual(), carpetas = a, evid = .schema_req_evidencia(), coms = .schema_req_comentarios()))), 8L)
  # Writing on a broken table works too, and the cycle stays refused.
  reiniciar()
  assign(paste0(CID, "/", REQ_KEYS$carpetas), a_mano(c("A", "B"), padre = c("B", "A")), envir = .mem)
  expect_null(err(crear("Nueva", "B")))
  expect_identical(err(requisiciones_carpeta_editar(CID, "A", list(padre_id = "B"), "d")), "requisiciones.error.carpeta_ciclo")
  expect_identical(nrow(tabla("carpetas")), 3L)
})

test_that("empresas: lista por id, NA = todas; JSON roto, vacío o con ids que ya no existen = todas", {
  reiniciar()
  expect_identical(err(crear("Sin", empresas = character())), "requisiciones.error.carpeta_empresas")
  x <- crear("Dos", empresas = c("e2", "e1", "e2"))
  expect_identical(fila(x)$empresas, "[\"e1\",\"e2\"]")
  expect_identical(.req_carpeta_empresas(fila(x)$empresas), c("e1", "e2"))
  for (malo in list(NA, "", "  ", "[]", "{roto", "null", "[1,2]"))
    expect_null(.req_carpeta_empresas(malo))
  expect_null(.req_carpeta_empresas("[\"viejo\"]", existentes = c("e1", "e2")))
  expect_identical(.req_carpeta_empresas("[\"viejo\",\"e1\"]", existentes = "e1"), c("viejo", "e1"))
  requisiciones_carpeta_editar(CID, x, list(empresas = NA), "d")
  expect_true(is.na(fila(x)$empresas))
})

test_that("burbujas: se ve si es de todas o comparte una empresa activa, y su madre se ve", {
  t <- a_mano(c("T", "N", "N1", "N2", "R", "M"), padre = c(NA, NA, "N", "N", NA, NA),
              empresas = c(NA, "[\"ncs\"]", NA, "[\"nts\"]", "{roto", "[\"borrada\"]"))
  a <- requisiciones_carpetas_arbol(t)
  vis <- function(activas) setNames(requisiciones_carpetas_visibles(a, activas, existentes = c("ncs", "nts")), a$id)
  v <- vis(c("ncs", "nts"))
  expect_true(all(v))
  v <- vis("nts")
  expect_false(v[["N"]]); expect_false(v[["N1"]]); expect_false(v[["N2"]])   # with its mother
  expect_true(v[["T"]]); expect_true(v[["R"]]); expect_true(v[["M"]])
  v <- vis(character())                                                        # every bubble off
  expect_identical(names(v)[v], c("T", "R", "M"))
  expect_true(all(requisiciones_carpetas_visibles(a, activas = NULL)))        # no filter
})

test_that("resumen: cuenta y suma el subárbol solo en la moneda de la carpeta; otra moneda no se suma ni se esconde", {
  reiniciar()
  a <- crear("Obra", presupuesto = 100000, moneda = "MXN"); h <- crear("Importados", a)
  requisicion_crear(CID, modifyList(base_req, list(carpeta_id = a, monto = 1000)), "d")
  requisicion_crear(CID, modifyList(base_req, list(carpeta_id = h, monto = 250)), "d")
  requisicion_crear(CID, modifyList(base_req, list(carpeta_id = h, monto = 40, moneda = "USD")), "d")
  requisicion_crear(CID, modifyList(base_req, list(monto = 7)), "d")          # General
  r <- requisiciones_carpetas_resumen(tabla("carpetas"), requisiciones_vista(CID))
  expect_identical(r$n_requisiciones[r$id == a], 3L)
  expect_equal(r$comprometido[r$id == a], 1250)
  expect_identical(r$n_otra_moneda[r$id == a], 1L)
  expect_identical(r$n_requisiciones[r$id == h], 2L)
  v <- requisiciones_vista(CID)
  expect_identical(sort(v$carpeta_ruta[!is.na(v$carpeta_ruta)]), c("Obra", "Obra › Importados", "Obra › Importados"))
})

test_that("General es virtual: carpeta_id NA, nunca una fila en S3", {
  reiniciar()
  r <- requisicion_crear(CID, base_req, "dani")
  expect_true(is.na(tabla("requisiciones")$carpeta_id))
  expect_null(tabla("carpetas"))
  a <- crear("Obra")
  requisiciones_mover(CID, r$id, a, "d"); requisiciones_mover(CID, r$id, NA, "d")
  expect_identical(tabla("carpetas")$id, a)
  expect_true(is.na(tabla("requisiciones")$carpeta_id))
})


# ═════════════════════════════════════════════════════════════════════════════
# T61 (Luis): requisition -> folder -> companies. A requisition's company is
# one of its folder's, compared by company id, never by text.
# ═════════════════════════════════════════════════════════════════════════════
catalogo <- function(cid = CID) {
  e <- .normalize(tibble::tibble(
    id = c("e1", "e2", "e3", "e4"), initials = c("NCS", "NTS", "VIE", "BOR"),
    razon_social = c("Networks Cargo Solutions, S.A. de C.V.", "Networks Transport Services SA de CV",
                     "Vieja Empresa", "Borrada SA"),
    nombre_corto = c("Networks Cargo", "Networks Transport", "Vieja", "Borrada"),
    nombres_alt = c("[\"NetCargo\"]", NA, NA, NA),
    activa = c(TRUE, TRUE, FALSE, TRUE), deleted = c(FALSE, FALSE, FALSE, TRUE),
    iniciales_anteriores = c("[\"NC\"]", NA, NA, NA)), .schema_empresas)
  assign(paste0(cid, "/", S3_KEYS$empresas), e, envir = .mem)
  e
}
req <- function(...) modifyList(base_req, list(...))

test_that("empresa_id_de: iniciales, anteriores, nombre, razón social y alias, exacto; nunca aproxima", {
  e <- catalogo()
  expect_identical(empresa_id_de(c("NCS", " ncs ", "NC", "Networks Cargo", "NETWORKS CARGO SOLUTIONS SA DE CV",
                                   "netcargo", "NTS"), e), c("e1", "e1", "e1", "e1", "e1", "e1", "e2"))
  expect_identical(empresa_id_de("VIE", e), "e3")                          # inactive: still a company
  expect_true(all(is.na(empresa_id_de(c("BOR", "Borrada", "", NA, "Networks", "NCS2", "Networks Cargo Sol"), e))))
  # Two companies answering to the same text: none (never a guess).
  e2 <- e; e2$nombre_corto[2] <- "Networks Cargo"
  expect_true(is.na(empresa_id_de("Networks Cargo", e2)))
  expect_identical(empresa_id_de("NCS", e2), "e1")
  expect_true(all(is.na(empresa_id_de(c("NCS", "x"), .schema_empresas()))))
})

test_that("crear: la empresa debe ser de la carpeta (por id); se guarda con sus iniciales; General acepta todas", {
  reiniciar(); catalogo()
  solo_ncs <- crear("Solo NCS", empresas = "e1")
  antes <- tabla("requisiciones")
  expect_identical(err(requisicion_crear(CID, req(empresa = "NTS", carpeta_id = solo_ncs), "d")),
                   "requisiciones.error.empresa_fuera_carpeta")
  expect_identical(err(requisicion_crear(CID, req(empresa = "Nadie SA", carpeta_id = solo_ncs), "d")),
                   "requisiciones.error.empresa_no_reconocida")
  expect_identical(tabla("requisiciones"), antes)
  r <- requisicion_crear(CID, req(empresa = "Networks Cargo", carpeta_id = solo_ncs), "d")
  expect_identical(tabla("requisiciones")$empresa[tabla("requisiciones")$id == r$id], "NCS")
  g <- requisicion_crear(CID, req(empresa = "nts"), "d")                    # General
  expect_identical(tabla("requisiciones")$empresa[tabla("requisiciones")$id == g$id], "NTS")
  todas <- crear("De todas")
  expect_null(err(requisicion_crear(CID, req(empresa = "VIE", carpeta_id = todas), "d")))
})

test_that("se valida sobre la tabla de carpetas recién leída: otra sesión le quitó la empresa", {
  reiniciar(); catalogo()
  c1 <- crear("Obra", empresas = c("e1", "e2"))
  k <- paste0(CID, "/", REQ_KEYS$carpetas); t <- .mem[[k]]
  t$empresas[t$id == c1] <- "[\"e2\"]"; assign(k, t, envir = .mem)
  expect_identical(err(requisicion_crear(CID, req(empresa = "NCS", carpeta_id = c1), "d")),
                   "requisiciones.error.empresa_fuera_carpeta")
})

test_that("una carpeta que pierde empresas no reescribe requisiciones: solo se marcan; editar otra cosa no obliga", {
  reiniciar(); catalogo()
  c1 <- crear("Obra", empresas = c("e1", "e2"))
  r <- requisicion_crear(CID, req(empresa = "NTS", carpeta_id = c1), "d")
  reqs_antes <- tabla("requisiciones")
  requisiciones_carpeta_editar(CID, c1, list(empresas = "e1"), "d")
  expect_identical(tabla("requisiciones"), reqs_antes)
  v <- requisiciones_vista(CID)
  expect_identical(v$empresa_aviso, "fuera_carpeta"); expect_identical(v$empresa_id, "e2")
  # Editing the title is fine and leaves the company alone.
  expect_null(err(requisicion_editar(CID, r$id, list(titulo = "Otro título"), "d")))
  expect_identical(tabla("requisiciones")$empresa, "NTS")
  # Changing the company to another one outside, or moving it to a folder
  # without its company, is refused; correcting it works.
  expect_identical(err(requisicion_editar(CID, r$id, list(empresa = "VIE"), "d")), "requisiciones.error.empresa_fuera_carpeta")
  c2 <- crear("Solo VIE", empresas = "e3")
  expect_identical(err(requisicion_editar(CID, r$id, list(carpeta_id = c2), "d")), "requisiciones.error.empresa_fuera_carpeta")
  expect_null(err(requisicion_editar(CID, r$id, list(empresa = "Networks Cargo"), "d")))
  expect_identical(tabla("requisiciones")$empresa, "NCS")
  expect_true(is.na(requisiciones_vista(CID)$empresa_aviso))
})

test_that("mover varias: si una no es de una empresa de la carpeta destino no se mueve ninguna, y dice cuántas", {
  reiniciar(); catalogo()
  a <- requisicion_crear(CID, req(empresa = "NCS"), "d"); b <- requisicion_crear(CID, req(empresa = "NTS"), "d")
  solo_ncs <- crear("Solo NCS", empresas = "e1")
  antes <- tabla("requisiciones")
  e <- tryCatch(requisiciones_mover(CID, c(a$id, b$id), solo_ncs, "d"), req_error = identity)
  expect_identical(e$clave, "requisiciones.error.empresa_fuera_mover"); expect_match(conditionMessage(e), "^1 ")
  expect_identical(tabla("requisiciones"), antes)
  expect_identical(requisiciones_mover(CID, a$id, solo_ncs, "d"), a$id)
  requisiciones_mover(CID, c(a$id, b$id), NA, "d")                          # General: every company
  expect_true(all(is.na(tabla("requisiciones")$carpeta_id)))
})

test_that("lista: empresa no reconocida se marca; sin catálogo no se marca nada ni se exige (como hoy)", {
  reiniciar()
  r <- requisicion_crear(CID, req(empresa = "Texto libre"), "d")            # no catalog yet
  expect_true(is.na(requisiciones_vista(CID)$empresa_aviso))
  catalogo()
  expect_identical(requisiciones_vista(CID)$empresa_aviso, "no_reconocida")
  expect_identical(tabla("requisiciones")$empresa, "Texto libre")          # never rewritten
})

test_that("desde provisión: el nombre completo se guarda como iniciales; sin empresa, no reconocida", {
  reiniciar(); catalogo()
  poner_provision("p1"); poner_provision("p2")
  k <- paste0(CID, "/", S3_KEYS$pasivos_provisions); p <- .mem[[k]]
  p$empresa[p$id == "p1"] <- "Networks Transport"; p$empresa[p$id == "p2"] <- NA; assign(k, p, envir = .mem)
  ids <- requisiciones_desde_provisiones(CID, "p1", NA, "d")
  expect_identical(tabla("requisiciones")$empresa[tabla("requisiciones")$id == ids], "NTS")
  expect_identical(err(requisiciones_desde_provisiones(CID, "p2", NA, "d")), "requisiciones.error.empresa_no_reconocida")
})

test_that("empresa no reconocida: a General sí se puede mover (lugar seguro); a una carpeta concreta no, hasta corregirla", {
  reiniciar()
  obra <- crear("Obra"); otra <- crear("Otra")
  r <- requisicion_crear(CID, req(empresa = "Texto libre", carpeta_id = obra), "d")    # before the catalog
  s <- requisicion_crear(CID, req(empresa = "Otro texto", carpeta_id = obra), "d")
  catalogo()
  expect_identical(requisiciones_vista(CID)$empresa_aviso, c("no_reconocida", "no_reconocida"))
  antes <- tabla("requisiciones")
  expect_identical(err(requisiciones_mover(CID, r$id, otra, "d")), "requisiciones.error.empresa_fuera_mover")
  expect_identical(err(requisicion_editar(CID, r$id, list(carpeta_id = otra), "d")), "requisiciones.error.empresa_no_reconocida")
  expect_identical(tabla("requisiciones"), antes)
  # To General: by "Mover a carpeta..." and by the form, the company untouched.
  expect_identical(requisiciones_mover(CID, r$id, NA, "d"), r$id)
  expect_null(err(requisicion_editar(CID, s$id, list(carpeta_id = NA), "d")))
  t <- tabla("requisiciones")
  expect_true(all(is.na(t$carpeta_id))); expect_identical(t$empresa, c("Texto libre", "Otro texto"))
  # Choosing a company still has to be a real one, also in General.
  expect_identical(err(requisicion_editar(CID, r$id, list(carpeta_id = NA, empresa = "Nadie"), "d")),
                   "requisiciones.error.empresa_no_reconocida")
  # Once corrected it can go into a folder again.
  requisicion_editar(CID, r$id, list(empresa = "NCS"), "d")
  expect_identical(requisiciones_mover(CID, r$id, otra, "d"), r$id)
})

cat("\n=== test_requisiciones_carpetas_arbol: fin ===\n")
