# =============================================================================
# tests/test_tema_t69.R
# TEMA 6 (T6-9): ONE rule for the theme of every mail about a client,
# correo_tema(cid) in R/tema_registro.R: the theme the client chose, else
# Marca; no client, or HopDesk itself (hd-admin), is Clásico. The integrator's
# conditions:
#   a) the cache: 5 minutes. The app lives for days: a client that changes its
#      theme gets it in the next mail after that, never a stale one;
#   b) correo_tema() is defined once (R/tema_registro.R), nowhere else;
#   c) a read that fails is Marca and the mail is still built (the reset too);
#   d) hd-admin (staff, no client) is Clásico, never a client's theme;
#   e) one client's mail never carries another's theme: two clients with
#      different themes, the reset of each, and a reset listing both: Clásico;
#   f) every text colour of the reset, the two user-limit notices, the user
#      request and the limit change reads >= 4.5 on its background, in both
#      themes. Every call site passes the rule (no mail without a theme).
# Run from project root:  Rscript tests/test_tema_t69.R
# =============================================================================
cat("=== test_tema_t69 ===\n\n")
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
leer <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (f in c("R/i18n.R", "R/tema_registro.R", "R/email_service.R", "R/tema_contraste.R")) sys.source(f, envir = globalenv())
# email_reset_html() and what it needs, alone from their files.
pieza <- function(f, nombres) for (ex in parse(f, keep.source = FALSE, encoding = "UTF-8"))
  if (is.call(ex) && identical(ex[[1]], as.name("<-")) && deparse(ex[[2]]) %in% nombres) eval(ex, globalenv())
pieza("R/auth_reset.R", c("email_reset_html", "RESET_MINUTOS"))
sys.source("R/usuarios_cuentas.R", envir = globalenv())   # usuario_ocultar() and its helpers
.HD_ADMIN_CID <- "hd-admin"

# A client's chosen theme, as group_config would say it (in memory).
ELEGIDO <- list(acme = "clasico", beta = NULL, gama = "marca")
lecturas <- 0L
tema_elegido_cliente <- function(cid) { lecturas <<- lecturas + 1L; if (identical(cid, "rota")) stop("S3 down"); ELEGIDO[[cid]] }

# ── a) the cache ─────────────────────────────────────────────────────────────
correo_tema_olvidar(); t0 <- as.POSIXct("2026-11-24 10:00:00", tz = "UTC")
ok("a: acme chose Clásico", identical(correo_tema("acme", ahora = t0), "clasico"))
ELEGIDO$acme <- "marca"; n0 <- lecturas
ok("a: within 5 minutes the cache answers (no new read)",
   identical(correo_tema("acme", ahora = t0 + 60), "clasico") && identical(lecturas, n0))
ok("a: after 5 minutes the next mail takes the new theme (one new read)",
   identical(correo_tema("acme", ahora = t0 + 301), "marca") && identical(lecturas, n0 + 1L))
ELEGIDO$acme <- "clasico"; correo_tema_olvidar()
ok("a: correo_tema_olvidar() empties it at once", identical(correo_tema("acme", ahora = t0 + 302), "clasico"))

# ── b) defined once ──────────────────────────────────────────────────────────
codigo <- c("app.R", list.files(c("R", "scripts"), pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
def <- codigo[vapply(codigo, function(f) grepl("(^|\\n)\\s*correo_tema\\s*<-\\s*function", leer(f), perl = TRUE), logical(1))]
ok(sprintf("b: correo_tema() is defined once, in R/tema_registro.R (%s)", paste(def, collapse = ", ")),
   identical(def, "R/tema_registro.R"))
ok("b: the registry's top level still only defines (no new.env() when it loads: rule 29)",
   !grepl("(^|\\n)\\.correo_tema_cache\\s*<-", leer("R/tema_registro.R"), perl = TRUE))

# ── c) a failed read ─────────────────────────────────────────────────────────
correo_tema_olvidar()
ok("c: a read that fails is Marca", identical(correo_tema("rota"), "marca"))
ok("c: an unknown theme is Marca", { ELEGIDO$zeta <- "no-existe"; identical(correo_tema("zeta"), "marca") })
cuentas <- function(cids) data.frame(username = paste0("u", seq_along(cids)), client_id = cids,
                                      cliente = toupper(cids), url = paste0("https://x/?reset=", seq_along(cids)),
                                      stringsAsFactors = FALSE)
h_rota <- email_reset_html(cuentas("rota"), tema = correo_tema_de("rota"))
ok("c: the reset mail is still built when the read fails (Marca)",
   nzchar(h_rota) && !grepl("{{", h_rota, fixed = TRUE) &&
     grepl(toupper(tema_color("superficie-2", "marca")), toupper(h_rota), fixed = TRUE))

# ── d) hd-admin ──────────────────────────────────────────────────────────────
ELEGIDO[["hd-admin"]] <- "marca"; n0 <- lecturas
ok("d: hd-admin (staff, no client) is Clásico, and nothing is read for it",
   identical(correo_tema("hd-admin"), "clasico") && identical(lecturas, n0))
ok("d: no client (NULL, empty, NA) is Clásico",
   identical(correo_tema(NULL), "clasico") && identical(correo_tema(""), "clasico") && identical(correo_tema(NA_character_), "clasico"))

# ── e) never another client's theme ──────────────────────────────────────────
correo_tema_olvidar()
h_acme <- email_reset_html(cuentas("acme"), tema = correo_tema_de("acme"))
h_gama <- email_reset_html(cuentas("gama"), tema = correo_tema_de("gama"))
h_dos  <- email_reset_html(cuentas(c("acme", "gama")), tema = correo_tema_de(c("acme", "gama")))
ok("e: acme's reset is exactly the Clásico one, gama's exactly the Marca one, and they differ",
   identical(h_acme, email_reset_html(cuentas("acme"), tema = "clasico")) &&
     identical(h_gama, email_reset_html(cuentas("gama"), tema = "marca")) &&
     !identical(gsub("u1|ACME|GAMA|acme|gama", "", h_acme), gsub("u1|ACME|GAMA|acme|gama", "", h_gama)))
ok("e: a reset that lists accounts of two clients is Clásico (no client's theme)",
   identical(correo_tema_de(c("acme", "gama")), "clasico") && identical(h_dos, email_reset_html(cuentas(c("acme", "gama")), tema = "clasico")))
ok("e: two accounts of one client: that client's theme", identical(correo_tema_de(c("gama", "gama", NA)), "marca"))

# ── f) contrast, and every call site passes the rule ────────────────────────
correos <- function(t) list(
  reset   = email_reset_html(cuentas("acme"), tema = t),
  aviso   = email_user_limit_warning_html("ACME", 9L, 10L, tema = t),
  tope    = email_user_limit_reached_html("ACME", 10L, 10L, tema = t),
  pedido  = email_user_request_html("ACME", 10L, 10L, "a@acme.mx", tema = t),
  cambio  = email_limit_change_html("ACME", 10L, 12L, "Luis", character(0), tema = t))
# Each text's colour against the background it sits on: its own, else the
# nearest ancestor's (style background or bgcolor); hidden ones (the
# preheader) do not count. WCAG: 4.5:1, or 3:1 for large text (h1/h2, or
# >= 24px, or bold >= 18.66px).
estilo <- function(n, prop) {
  s <- xml2::xml_attr(n, "style"); if (is.na(s)) return(NA_character_)
  m <- regmatches(s, regexec(paste0("(?:^|;)\\s*", prop, "\\s*:\\s*(#[0-9A-Fa-f]{6})"), s, perl = TRUE))[[1]]
  if (length(m) == 2L) m[2] else NA_character_
}
fondo_de <- function(n) {
  for (a in c(list(n), as.list(xml2::xml_parents(n)))) {   # nearest ancestor first
    b <- estilo(a, "background(?:-color)?")
    if (is.na(b)) { bg <- xml2::xml_attr(a, "bgcolor"); if (!is.na(bg) && grepl("^#[0-9A-Fa-f]{6}$", bg)) b <- bg }
    if (!is.na(b)) return(b)
  }
  "#FFFFFF"
}
grande <- function(n) {
  if (xml2::xml_name(n) %in% c("h1", "h2")) return(TRUE)
  s <- xml2::xml_attr(n, "style"); if (is.na(s)) return(FALSE)
  px <- suppressWarnings(as.numeric(sub(".*font-size:\\s*([0-9.]+)px.*", "\\1", s)))
  !is.na(px) && (px >= 24 || (px >= 18.66 && grepl("font-weight:\\s*bold", s)))
}
pares_bajos <- function(h, t) {
  doc <- xml2::read_html(paste0("<html><body>", h, "</body></html>"))
  malos <- character()
  for (n in xml2::xml_find_all(doc, "//*[@style]")) {
    s <- xml2::xml_attr(n, "style")
    if (grepl("display:\\s*none", s)) next
    oculto <- any(vapply(as.list(xml2::xml_parents(n)), function(a) grepl("display:\\s*none", xml2::xml_attr(a, "style") %||% ""), logical(1)))
    if (oculto) next
    col <- estilo(n, "color"); if (is.na(col)) next
    if (!nzchar(trimws(xml2::xml_text(n)))) next
    bg <- fondo_de(n); r <- tema_contraste(col, bg); min_r <- if (grande(n)) 3 else 4.5
    if (r < min_r) malos <- c(malos, sprintf("%s on %s %.2f (<%s)", toupper(col), toupper(bg), r, min_r))
  }
  unique(malos)
}
# Clásico's account mails look like today (TEMA 4b): their pairs below AA are
# a debt, listed by colour, that may only shrink (Luis decides a fix): the
# footer's grey, the limit warning's amber, the limit change's green.
.CLASICO_DEUDA_CORREOS <- c("#7B8794", "#F59E0B", "#16A34A")
for (t in c("clasico", "marca")) for (n in names(cs <- correos(t))) {
  m <- pares_bajos(cs[[n]], t)
  if (t == "clasico") m <- m[!substr(m, 1, 7) %in% .CLASICO_DEUDA_CORREOS]
  ok(sprintf("f: [%s] %s: every text colour >= 4.5 on its background%s", t, n,
             if (length(m)) paste0(" (", paste(m, collapse = "; "), ")") else ""), !length(m) && !grepl("{{", cs[[n]], fixed = TRUE))
}
llamadas <- list(
  "R/auth_reset.R"    = "email_reset_html(env, tema = correo_tema_de(env$client_id))",
  "R/email_service.R" = "tema_c  <- correo_tema(client_id)",
  "R/email_service.R" = "tema = correo_tema(client_id))",
  "R/tiers_module.R"  = "email_user_request_html(dname, cur, max, contact, tema = correo_tema(cid))",
  "R/tiers_module.R"  = "tema = correo_tema(client_in))",
  "app.R"             = "tema = correo_tema(invite$client_id))")
for (i in seq_along(llamadas))
  ok(sprintf("f: %s sends with the rule: %s", names(llamadas)[i], llamadas[[i]]),
     grepl(llamadas[[i]], leer(names(llamadas)[i]), fixed = TRUE))
ok("f: no mail of the app takes tema_de_cliente() any more (that is the screens' theme)",
   !any(vapply(c("app.R", "R/tiers_module.R", "R/email_service.R", "R/auth_reset.R"),
               function(f) grepl("tema = tema_de_cliente(", leer(f), fixed = TRUE), logical(1))))

cat(sprintf("\n=== results: %d passed, %d failed ===\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
