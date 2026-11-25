# =============================================================================
# tests/test_tema_correo_semanal.R
# TEMA stage 5a: the weekly mails (R/avisos_correos.R) follow the theme the
# client CHOSE; a client that chose none gets Marca, as every client did
# before. The job (scripts/avisos_job.R) puts correo_tema(cid) in the mail's
# data: the cid it is working on, never CLIENT_ID.
#   1. two clients of different themes in ONE run: each mail in its client's
#      theme (acme chose Clásico, beta chose nothing -> Marca);
#   2. correo_tema() reads each client once per run (cache keyed by cid: two
#      clients keep two values), and a read that fails is Marca and the mails
#      still go out;
#   3. Friday's kept histories (historial, partidas) are identical with and
#      without a theme: the theme is never stored;
#   4. the 22 mails of t45's scenarios: "chose Marca" is byte for byte "chose
#      nothing" (and test_tema_avisos_correos_iguales holds "chose nothing" =
#      the base); under Clásico no Marca-only tone is left;
#   5. Clásico's text pairs reach 4.5:1 (arena_tinta/arena, gris_claro/marino,
#      revision_tinta/revision, tinta/fondo); the chart's bars do not count.
# The job's fixture: in-memory S3, two active clients, a fake Resend.
# Run from the project root:  Rscript tests/test_tema_correo_semanal.R
# =============================================================================
cat("=== test_tema_correo_semanal ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, cond) {
  r <- tryCatch(cond, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
source("tests/helpers/avisos_job_fixture.R")

# The fake Resend, keeping the HTML too (the fixture's keeps to/subject only).
send_email <- function(to, subject, html, queue_on_fail = TRUE) {
  fx_enviados[[length(fx_enviados) + 1L]] <<- list(to = to, subject = subject, html = html, cola = queue_on_fail)
  structure(TRUE, resend_id = paste0("re_", length(fx_enviados)))
}
de <- function(cid) Filter(function(x) grepl(paste0("@", cid, "-"), x$to, fixed = TRUE), fx_enviados)
html_de <- function(cid) paste(vapply(de(cid), `[[`, "", "html"), collapse = "\n")
elige <- function(cid, tema) fx_pon(cid, S3_KEYS$group_config, modifyList(.schema_group_config(), list(tema = tema)))

MARCA   <- unlist(.correo_colores("marca"));  CLASICO <- unlist(.correo_colores("clasico"))
SOLO_MARCA <- setdiff(toupper(MARCA), toupper(CLASICO))   # Marca's tones Clásico never uses
tiene <- function(h, hex) any(vapply(hex, function(x) grepl(x, h, ignore.case = TRUE, fixed = FALSE), logical(1)))

lunes   <- function() job_correr("lunes", FALSE, hoy = FX_LUNES, ahora = FX_AHORA_L)
viernes <- function() job_correr("viernes", FALSE, hoy = FX_VIERNES, ahora = FX_AHORA_V)

# ── 1 + 2: two clients, two themes, one run; one read per client ─────────────
fx_reset(); correo_tema_olvidar()
elige("acme", "clasico")                     # beta chose nothing
lecturas <- list()
original <- tema_elegido_cliente
tema_elegido_cliente <- function(cid) { lecturas[[cid]] <<- (lecturas[[cid]] %||% 0L) + 1L; original(cid) }
l <- lunes()
ok("1: the run sends both clients' mails", length(de("acme")) > 0L && length(de("beta")) > 0L)
ok("1: acme (chose Clásico): Clásico's primario, no Marca-only tone",
   grepl(CLASICO[["marino"]], html_de("acme"), ignore.case = TRUE) && !tiene(html_de("acme"), SOLO_MARCA))
ok("1: beta (chose nothing): Marca, as before (its lino background)",
   grepl(MARCA[["fondo"]], html_de("beta"), ignore.case = TRUE))
ok(sprintf("2: one read per client per run, though each has %d recipients (acme %s, beta %s)",
           length(de("acme")), lecturas$acme %||% 0L, lecturas$beta %||% 0L),
   identical(lecturas$acme, 1L) && identical(lecturas$beta, 1L) && length(de("acme")) > 1L)
ok("2: the cache is keyed by cid: two clients keep two values",
   identical(correo_tema("acme"), "clasico") && identical(correo_tema("beta"), "marca") &&
   identical(lecturas$acme, 1L))
tema_elegido_cliente <- original

fx_reset(); correo_tema_olvidar()
elige("acme", "clasico")
tema_elegido_cliente <- function(cid) stop("group_config unreadable")
l <- lunes()
ok("2: a read that fails never stops a mail: every recipient still gets one",
   identical(fx_cuenta(l, "enviado"), length(fx_enviados)) && length(de("acme")) > 0L && length(de("beta")) > 0L)
ok("2: ...and that client's mail is Marca", grepl(MARCA[["fondo"]], html_de("acme"), ignore.case = TRUE))
tema_elegido_cliente <- original
# TEMA 6-9 (one rule for every mail): a mail with no client is Clásico; the
# weekly job always passes a client's cid, so its mails stay as they were.
ok("2: no cid or an empty one is Clásico (a mail with no client)",
   identical(correo_tema(NULL), "clasico") && identical(correo_tema(""), "clasico"))

# ── 3: Friday keeps the same histories with and without a theme ─────────────
guardado <- function() {
  ks <- c(.s3_key(.HISTORIAL_KEY, client_id = "acme"), .s3_key(.PARTIDAS_KEY, client_id = "acme"))
  vapply(ks, function(k) if (exists(k, envir = .mem)) digest_txt(get(k, envir = .mem)) else "(ninguno)", "")
}
# A Friday with a close: SAP refreshed after Monday (else nothing is kept).
semana <- function(tema = NULL) {
  fx_reset(); correo_tema_olvidar()
  if (!is.null(tema)) elige("acme", tema)
  invisible(lunes())
  for (cid in c("acme", "beta")) fx_cliente(cid, FX_AHORA_V - 3600)
  if (!is.null(tema)) elige("acme", tema)
  fx_enviados <<- list()
  invisible(viernes())
  guardado()
}
sin_tema <- semana(); con_tema <- semana("clasico")
ok("3: Friday's historial and partidas are kept, identical with and without a theme",
   all(sin_tema != "(ninguno)") && identical(sin_tema, con_tema))
ok("3: Friday's mail itself follows the theme (acme in Clásico)",
   length(de("acme")) > 0L && !tiene(html_de("acme"), SOLO_MARCA))

# ── 4: the 22 mails of t45's scenarios, by theme ──────────────────────────────
correos <- function(tema) callr::r(function(t45, tema) {
  .hd_capturados <- list()
  envolver <- function(fn) {
    orig <- get(fn, envir = globalenv())
    nuevo <- function(d, dest) {
      if (!is.null(tema)) d$tema <- tema
      h <- orig(d, dest)
      .hd_capturados[[length(.hd_capturados) + 1L]] <<- paste(as.character(h), collapse = "")
      h
    }
    attr(nuevo, "hd_envuelta") <- TRUE
    assign(fn, nuevo, envir = globalenv())
  }
  sink(tempfile()); on.exit(sink(), add = TRUE)
  for (e in parse(t45, keep.source = FALSE)) {
    # Never t45's own ending (if (.fail > 0L) quit(...)): under Clásico its
    # Marca checks fail on purpose, and a quit() would end this process.
    if (any(grepl("quit(", deparse(e), fixed = TRUE))) next
    try(eval(e, globalenv()), silent = TRUE)
    for (fn in c("correo_lunes_html", "correo_viernes_html"))
      if (exists(fn, envir = globalenv(), inherits = FALSE) && is.null(attr(get(fn, envir = globalenv()), "hd_envuelta")))
        envolver(fn)
  }
  unlist(.hd_capturados)
}, args = list(t45 = normalizePath("tests/test_t45_correos_semana.R", winslash = "/"), tema = tema))
nada <- correos(NULL); marca <- correos("marca"); clasico <- correos("clasico")
ok(sprintf("4: t45's scenarios build the weekly mails (%d)", length(nada)), length(nada) >= 12L)
ok("4: \"chose Marca\" is byte for byte \"chose nothing\" (all of them)", identical(nada, marca))
ok("4: under Clásico no Marca-only tone is left in any mail", !any(vapply(clasico, tiene, logical(1), hex = SOLO_MARCA)))
ok("4: under Clásico the mails are not Marca's", !identical(clasico, nada))

# ── 5: Clásico's text pairs ───────────────────────────────────────────────────
lum <- function(h) { v <- strtoi(substring(sub("^#", "", h), c(1, 3, 5), c(2, 4, 6)), 16L) / 255
  v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4); sum(c(0.2126, 0.7152, 0.0722) * v) }
contraste <- function(a, b) { l <- sort(c(lum(a), lum(b)), decreasing = TRUE); (l[1] + 0.05) / (l[2] + 0.05) }
pares <- list(c("arena_tinta", "arena"), c("gris_claro", "marino"), c("revision_tinta", "revision"), c("tinta", "fondo"))
for (p in pares) {
  r <- contraste(CLASICO[[p[1]]], CLASICO[[p[2]]])
  ok(sprintf("5: Clásico %s on %s: %.2f:1 >= 4.5", p[1], p[2], r), r >= 4.5)
}
# TEMA 6-9 (the Coordinador's readability decision): "reprogramado" reads on
# the mail's paper in both themes.
for (tt in c("marca", "clasico")) {
  cc <- unlist(.correo_colores(tt)); r <- contraste(cc[["reprog"]], cc[["papel"]])
  ok(sprintf("5: %s reprogramado (%s) on papel: %.2f:1 >= 4.5", tt, cc[["reprog"]], r), r >= 4.5)
}
ok("5: Clásico's roles are its own: collections gold (acento), payments navy (primario)",
   identical(toupper(CLASICO[["dorado"]]), toupper(tema_color("acento", "clasico"))) &&
   identical(toupper(CLASICO[["marino"]]), toupper(tema_color("primario", "clasico"))))
ok("a theme the registry does not know is Marca whole, never a mix (and \" Marca \" is Marca)",
   identical(.correo_colores("no-existe"), .correo_colores("marca")) &&
   identical(.correo_colores(" Marca "), .correo_colores("marca")))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
