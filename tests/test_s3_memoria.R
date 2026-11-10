# =============================================================================
# tests/test_s3_memoria.R
# The in-memory S3 (tests/helpers/s3_memoria.R) keeps aws.s3's contract for
# the six functions the app uses (PENDIENTES §4: put_object, get_object and
# get_bucket used to reach the network from a test):
#   1. put_object / get_object / get_bucket / s3readRDS / s3saveRDS, together;
#   2. failures on purpose: read (.mem_falla), write (.mem_falla_escritura,
#      nothing stored or counted) and listing (.mem_falla_lista);
#   3. through the app's own code: an attachment uploaded and read back
#      (R/adjuntos_s3.R), the payments log read across its chunks
#      (get_bucket in R/pagos_generados_persistence.R);
#   4. s3_memoria_restaurar() puts the six real functions back.
#
# Run from project root:  Rscript tests/test_s3_memoria.R
# =============================================================================
cat("=== test_s3_memoria ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
reales <- lapply(stats::setNames(nm = c("s3readRDS", "s3saveRDS", "object_exists", "put_object", "get_object",
                                        "get_bucket")), function(nm) get(nm, asNamespace("aws.s3")))
source("tests/helpers/s3_memoria.R")
falla <- function(expr, patron) {
  m <- tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
  !is.na(m) && grepl(patron, m)
}
ok("las seis funciones de aws.s3 que usa la app están reemplazadas",
   all(vapply(names(reales), function(nm) !identical(get(nm, asNamespace("aws.s3")), reales[[nm]]), logical(1))))

# ── 1. The contract ──────────────────────────────────────────────────────────
s3_memoria_reset()
df <- data.frame(id = c("a", "b"), monto = c(1.5, 2))
tf <- tempfile(fileext = ".rds"); saveRDS(df, tf)
ok("put_object de un .rds devuelve TRUE y cuenta como escritura",
   isTRUE(aws.s3::put_object(file = tf, object = "acme/tabla.rds", bucket = "b")) && .mem_escrituras == 1L)
ok("… y s3readRDS lo lee como el objeto de R (el respaldo de .s3_write, la cola fuera de línea)",
   identical(aws.s3::s3readRDS(object = "acme/tabla.rds", bucket = "b"), df))
bytes <- as.raw(c(0x25, 0x50, 0x44, 0x46, 0x00, 0xff))
ok("put_object de bytes (un adjunto) y get_object devuelve los mismos bytes",
   isTRUE(aws.s3::put_object(file = bytes, object = "acme/adj/x.pdf", bucket = "b",
                             headers = list(`Content-Type` = "application/pdf"))) &&
     identical(aws.s3::get_object(object = "acme/adj/x.pdf", bucket = "b"), bytes))
aws.s3::s3saveRDS(df, object = "acme/otra.rds", bucket = "b")
ok("get_object de algo guardado con s3saveRDS da sus bytes .rds (se leen de vuelta igual)",
   { r <- aws.s3::get_object(object = "acme/otra.rds", bucket = "b"); tf2 <- tempfile(); writeBin(r, tf2)
     is.raw(r) && identical(readRDS(tf2), df) })
for (k in c("acme/pagos_generados_002.rds", "acme/pagos_generados.rds", "acme/pagos_generados_001.rds", "beta/pagos_generados.rds"))
  aws.s3::s3saveRDS(df, object = k, bucket = "b")
l <- aws.s3::get_bucket(bucket = "b", prefix = "acme/pagos_generados", max = 200L)
ok("get_bucket: solo las llaves con el prefijo, en orden, y cada objeto con [[\"Key\"]]",
   identical(unname(vapply(l, function(o) o[["Key"]], character(1))),
             c("acme/pagos_generados.rds", "acme/pagos_generados_001.rds", "acme/pagos_generados_002.rds")) &&
     all(vapply(l, function(o) is.numeric(o$Size) && o$Size > 0, logical(1))))
ok("get_bucket respeta max", length(aws.s3::get_bucket(bucket = "b", prefix = "acme/", max = 2L)) == 2L)
ok("get_bucket de un prefijo sin nada: lista vacía", !length(aws.s3::get_bucket(bucket = "b", prefix = "zeta/")))
ok("una llave que no existe: NoSuchKey (get_object y s3readRDS)",
   falla(aws.s3::get_object(object = "nada", bucket = "b"), "NoSuchKey") &&
     falla(aws.s3::s3readRDS(object = "nada", bucket = "b"), "NoSuchKey"))
ok("object_exists", isTRUE(aws.s3::object_exists("acme/adj/x.pdf", bucket = "b")) &&
     !isTRUE(aws.s3::object_exists("nada", bucket = "b")))

# ── 2. Failures on purpose ───────────────────────────────────────────────────
.mem_falla <- "acme/adj/x.pdf"
ok("lectura que falla (.mem_falla): get_object no devuelve nada", falla(aws.s3::get_object(object = "acme/adj/x.pdf", bucket = "b"), "simulated"))
.mem_falla <- character()
.mem_falla_escritura <- c("acme/w.rds"); antes <- .mem_escrituras
ok("escritura que falla (.mem_falla_escritura), con put_object y con s3saveRDS: no guarda ni cuenta",
   falla(aws.s3::put_object(file = tf, object = "acme/w.rds", bucket = "b"), "write failure") &&
     falla(aws.s3::s3saveRDS(df, object = "acme/w.rds", bucket = "b"), "write failure") &&
     !exists("acme/w.rds", envir = .mem, inherits = FALSE) && .mem_escrituras == antes)
.mem_falla_lista <- "acme/pagos_generados"
ok("listado que falla (.mem_falla_lista)", falla(aws.s3::get_bucket(bucket = "b", prefix = "acme/pagos_generados"), "listing"))
s3_memoria_reset()
ok("s3_memoria_reset() limpia el almacén y las tres listas de fallas",
   !length(ls(.mem)) && !length(.mem_falla) && !length(.mem_falla_escritura) && !length(.mem_falla_lista) && .mem_escrituras == 0L)

# ── 3. Through the app's own code ────────────────────────────────────────────
for (e in parse("R/adjuntos_s3.R", keep.source = FALSE))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]]) %in% c(".adj_s3_poner", ".adj_s3_traer"))
    eval(e, globalenv())
ok("un adjunto subido con .adj_s3_poner() se lee igual con .adj_s3_traer()",
   { .adj_s3_poner(bytes, "acme/adjuntos/r1/f.pdf", "application/pdf"); identical(.adj_s3_traer("acme/adjuntos/r1/f.pdf"), bytes) })
.mem_falla_escritura <- "acme/adjuntos/r1/g.pdf"
ok("… y si S3 no guarda, .adj_s3_poner() avisa (stop), no anota un archivo que no está",
   falla(.adj_s3_poner(bytes, "acme/adjuntos/r1/g.pdf", "application/pdf"), "no se pudo subir"))
.mem_falla_escritura <- character()
suppressMessages(source("R/pagos_generados_persistence.R"))
pago <- function(id) { p <- .schema_pagos_generados()[1, , drop = FALSE]; p[] <- NA; p$id <- id; p }
aws.s3::s3saveRDS(pago("p-nuevo"), object = "acme/pagos_generados.rds", bucket = "b")
aws.s3::s3saveRDS(pago("p-viejo"), object = "acme/pagos_generados_001.rds", bucket = "b")
aws.s3::s3saveRDS(pago("p-otro"), object = "beta/pagos_generados.rds", bucket = "b")
ok("read_pagos_generados() junta las partes de su cliente por get_bucket (no las de otro)",
   setequal(read_pagos_generados(client_id = "acme")$id, c("p-nuevo", "p-viejo")))

s3_memoria_reset()
aws.s3::put_object(file = tf, object = "acme/u1.rds", bucket = "b"); aws.s3::s3saveRDS(df, object = "acme/s1.rds", bucket = "b")
ok(".mem_subidas cuenta solo lo que subió put_object (y reset lo vuelve a 0)",
   .mem_subidas == 1L && .mem_escrituras == 2L && { s3_memoria_reset(); .mem_subidas == 0L })
utils::assignInNamespace("put_object", function(...) stop("DRY RUN"), "aws.s3")   # as a dry-run script does
s3_memoria_reinstalar()
ok("s3_memoria_reinstalar() vuelve a poner los seis falsos tras un código que cambió aws.s3",
   isTRUE(aws.s3::put_object(file = bytes, object = "acme/r.bin", bucket = "b")) &&
     identical(aws.s3::get_object(object = "acme/r.bin", bucket = "b"), bytes))

# ── 4. Back to the real ones ─────────────────────────────────────────────────
s3_memoria_restaurar()
ok("s3_memoria_restaurar() devuelve las seis funciones reales de aws.s3",
   all(vapply(names(reales), function(nm) identical(get(nm, asNamespace("aws.s3")), reales[[nm]]), logical(1))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
