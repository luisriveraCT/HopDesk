# =============================================================================
# tests/test_abono_caracterizacion_chromote.R
# Abono parcial in a real headless Chrome: what only real clicks and keys
# show. Step 0b of T25 recorded master's hand-made modal here (19/0); since
# the migration the same cases (B1..) run on hd_table "abono_parcial". The
# fixture is tests/helpers/abono_fixture_app.R (the real modal and server
# over tests/helpers/abono_datos_prueba.R; no S3). [CAMBIA] marks what
# reportes/T25_diseno.md changes on purpose; each one says how it was.
# Sizes: Luis's window (1293 x 632) and 1772 x 866; screenshots to
# AB_CAPTURAS if set.
#
# Run from project root:  Rscript tests/test_abono_caracterizacion_chromote.R
# =============================================================================
cat("=== test_abono_caracterizacion_chromote ===\n\n")

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x
source("tests/helpers/live_app_harness.R")

repo <- normalizePath(".", winslash = "/")
PORT <- 38994L
CAPTURAS <- Sys.getenv("AB_CAPTURAS", "")
app <- NULL; b <- NULL
on.exit({ try(if (!is.null(b)) b$close(), silent = TRUE); stop_bg_app(app) }, add = TRUE)
app <- tryCatch(start_bg_app(sprintf('setwd("%s"); source("tests/helpers/abono_fixture_app.R")$value', repo), PORT, timeout_s = 90),
                error = function(e) { message(conditionMessage(e)); NULL })
ok("fixture app started", !is.null(app))
if (is.null(app)) { cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n"); quit(status = 1L) }

b <- new_chromote_session()
pantalla <- function(w, h) invisible(b$Emulation$setDeviceMetricsOverride(width = w, height = h, deviceScaleFactor = 1, mobile = FALSE))
ev <- function(js) {
  r <- b$Runtime$evaluate(js, returnByValue = TRUE)
  if (!is.null(r$exceptionDetails)) stop("JS: ", r$exceptionDetails$exception$description %||% r$exceptionDetails$text)
  r$result$value
}
centro <- function(js_el) ev(sprintf("(function(){ var e = %s; e.scrollIntoView({block:'center', behavior:'instant'});
  var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", js_el))
raton <- function(tipo, p, botones = 0L)
  b$Input$dispatchMouseEvent(type = tipo, x = p[[1]], y = p[[2]], button = if (tipo == "mouseMoved" && !botones) "none" else "left",
                             buttons = botones, clickCount = 1L)
clic <- function(js_el) {
  p <- centro(js_el)
  raton("mouseMoved", p); raton("mousePressed", p, 1L); raton("mouseReleased", p)
  Sys.sleep(0.35)
}
TECLAS <- list(Enter = list(code = "Enter", vk = 13L), Escape = list(code = "Escape", vk = 27L))
tecla <- function(k) {
  d <- TECLAS[[k]]
  b$Input$dispatchKeyEvent(type = "rawKeyDown", key = k, code = d$code, windowsVirtualKeyCode = d$vk)
  b$Input$dispatchKeyEvent(type = "keyUp", key = k, code = d$code, windowsVirtualKeyCode = d$vk)
  Sys.sleep(0.35)
}
escribe <- function(texto) {          # over what the focused input holds
  ev("document.activeElement && document.activeElement.select && document.activeElement.select()")
  b$Input$insertText(text = texto); Sys.sleep(0.2)
}
espera <- function(f, s = 8) poll_until(function() isTRUE(tryCatch(f(), error = function(e) FALSE)), timeout_s = s)
eco <- function() jsonlite::fromJSON(ev("document.getElementById('eco').textContent") %||% "{}", simplifyVector = FALSE)
captura <- function(nombre) if (nzchar(CAPTURAS)) {
  dir.create(CAPTURAS, showWarnings = FALSE, recursive = TRUE)
  b$screenshot(file.path(CAPTURAS, nombre), show = FALSE)
}

# ── The modal on the engine (table ab_tabla-hdt; rows by id) ────────────────
T <- "ab_tabla-hdt"
id_de <- function(doc) c("F-101" = "AP|NCS|MXN|F-101", "F-102" = "AP|NCS|MXN|F-102", "F-103" = "AP|NCS|MXN|F-103",
                         "F-201" = "AP|NCS|USD|F-201", "F-501" = "AP|NTS|MXN|F-501", "F-403" = "AP|NTS|MXN|F-403")[[doc]]
doc_de <- function(ids) sub("^.*\\|", "", ids)
estado  <- function() ev(sprintf("HDT.estado('%s')", T))
abierto <- function() isTRUE(ev(sprintf("!!document.querySelector('.modal.show #%s')", T)))
# Opens the modal fresh: closing it and opening again gives the engine a new
# container (empty selection, no amounts typed).
abrir <- function() {
  if (abierto()) { clic("document.querySelector('.modal.show .hd-modal-cerrar')"); espera(function() !abierto()) }
  clic("document.getElementById('pick_abono')")
  espera(function() abierto() && length(unlist(estado()$visibles)) >= 6L, 15)
}
# The engine's groups open closed (one row with its sums); the cases that
# reach a grouped invoice open them all first.
abrir_grupos <- function() {
  if (any(grepl("^grupo:cerrado", lista()))) clic(sprintf("document.querySelector('#%s .hdt-grupos-todos')", T))
  espera(function() !any(grepl("^grupo:cerrado", lista())), 5)
}
fila      <- function(doc) sprintf("HDT.fila('%s', '%s')", T, id_de(doc))
casilla   <- function(doc) sprintf("%s.querySelector('input.hdt-cbx')", fila(doc))
td_importe <- function(doc) sprintf("(function(){ var tr = %s; return tr.children[tr.children.length - 1]; })()", fila(doc))
celda     <- function(doc) sprintf("%s.querySelector('td.hdt-editable, td.hdt-editando')", fila(doc))
texto_imp <- function(doc) ev(sprintf("%s.textContent", td_importe(doc)))
clases    <- function(doc) ev(sprintf("%s.className", td_importe(doc)))
grupo     <- sprintf("document.querySelector('#%s tr.hdt-grupo')", T)
seleccion <- function() doc_de(unlist(estado()$seleccion))
visibles  <- function() doc_de(unlist(estado()$visibles))
lista     <- function() unlist(estado()$lista)
buscar    <- function(q) {
  clic(sprintf("document.querySelector('#%s .hdt-buscar')", T))
  ev(sprintf("(function(){ var e = document.querySelector('#%s .hdt-buscar'); e.value = %s;
              e.dispatchEvent(new Event('input', {bubbles: true})); })()", T, jsonlite::toJSON(q, auto_unbox = TRUE)))
  Sys.sleep(0.4)
}
escribe_en <- function(doc, valor) { clic(celda(doc)); escribe(valor); tecla("Enter"); tecla("Escape") }
enviar    <- function() clic("document.getElementById('ab_stage')")
avisos    <- function(tipo) vapply(Filter(function(a) identical(a$tipo, tipo), eco()$avisos), `[[`, "", "texto")

for (tam in list(c(1293L, 632L), c(1772L, 866L))) {
  pantalla(tam[1], tam[2])
  et <- sprintf("[%dx%d]", tam[1], tam[2])
  b$Page$navigate(sprintf("http://127.0.0.1:%d/", PORT)); b$Page$loadEventFired()
  espera(function() ev("!!document.getElementById('pick_abono') && !!window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()"), 20)

  ok(paste(et, "[CAMBIA C8] B1 the modal opens with the 6 invoices and one group (Proveedor Uno, NCS, MXN), closed"), {
    abrir() && setequal(visibles(), c("F-101", "F-102", "F-103", "F-201", "F-501", "F-403")) &&
      sum(grepl("^grupo:", lista())) == 1L &&
      any(grepl("^grupo:cerrado:", lista())) &&           # [CAMBIA C8] opens closed (before: open)
      setequal(doc_de(strsplit(sub("^grupo:[a-z]+:", "", grep("^grupo:", lista(), value = TRUE)), ",")[[1]]),
               c("F-101", "F-102", "F-103"))
  })
  captura(sprintf("T25_abierto_%dx%d.png", tam[1], tam[2]))
  ok(paste(et, "[CAMBIA C1] B2 every amount can be typed in from the start and shows the balance (before: disabled until ticked)"), {
    abrir(); abrir_grupos()
    all(vapply(c("F-101", "F-103", "F-201"), function(d) grepl("hdt-editable", clases(d)), logical(1))) &&
      identical(texto_imp("F-103"), "$250.00") && identical(texto_imp("F-102"), "$2,500.50")
  })
  ok(paste(et, "[CAMBIA C1] B3 typing an amount ticks its row; unticking keeps what was typed"), {
    abrir(); abrir_grupos(); escribe_en("F-201", "500")
    a <- identical(seleccion(), "F-201") && grepl("hdt-editada", clases("F-201")) && identical(texto_imp("F-201"), "$500.00")
    clic(casilla("F-201"))
    a && !length(seleccion()) && identical(texto_imp("F-201"), "$500.00")
  })
  ok(paste(et, "B4 an amount over the balance: danger tone and 'Mayor al saldo ($250.00)'; it isn't sent"), {
    abrir(); abrir_grupos(); escribe_en("F-103", "300")
    p <- centro(td_importe("F-103")); raton("mouseMoved", c(p[[1]] - 3, p[[2]])); raton("mouseMoved", p)
    espera(function() !is.null(ev("(function(){ var t = document.querySelector('.hdt-tip'); return t && !t.hidden ? t.textContent : null; })()")), 3)
    tip <- ev("(function(){ var t = document.querySelector('.hdt-tip'); return t && !t.hidden ? t.textContent : null; })()")
    grepl("hdt-ed-mal", clases("F-103")) && grepl("Mayor al saldo ($250.00)", tip %||% "", fixed = TRUE)
  })
  ok(paste(et, "B5 the group's box ticks its 3 invoices; one off leaves it indeterminate"), {
    abrir(); abrir_grupos()
    clic(sprintf("%s.querySelector('input.hdt-cbx-grupo')", grupo))
    todas <- setequal(seleccion(), c("F-101", "F-102", "F-103"))
    clic(casilla("F-102"))
    ind <- isTRUE(ev(sprintf("%s.querySelector('input.hdt-cbx-grupo').indeterminate", grupo)))
    todas && ind && setequal(seleccion(), c("F-101", "F-103"))
  })
  ok(paste(et, "B6 the group's arrow and the engine's all-groups button open and close it"), {
    abrir()
    antes <- grepl("^grupo:abierto", grep("^grupo:", lista(), value = TRUE))
    clic(sprintf("%s.querySelector('button.hdt-grupo-abrir')", grupo))
    uno <- grepl("^grupo:abierto", grep("^grupo:", lista(), value = TRUE)) != antes
    clic(sprintf("document.querySelector('#%s .hdt-grupos-todos')", T))
    a1 <- grep("^grupo:", lista(), value = TRUE)
    clic(sprintf("document.querySelector('#%s .hdt-grupos-todos')", T))
    a2 <- grep("^grupo:", lista(), value = TRUE)
    uno && a1 != a2
  })
  ok(paste(et, "B7 search: by reference (A-77), by Parte (dos), and back"), {
    abrir()
    buscar("a-77"); s1 <- identical(visibles(), "F-102")
    buscar("dos");  s2 <- identical(visibles(), "F-201")
    buscar("");     s1 && s2 && length(visibles()) == 6L
  })
  ok(paste(et, "[CAMBIA C5] B8 Enviar with nothing ticked: a warning (dictionary), nothing sent, the modal stays (before: alert())"), {
    abrir(); n0 <- length(avisos("warning"))
    enviar()
    espera(function() length(avisos("warning")) > n0) &&
      identical(tail(avisos("warning"), 1), "Selecciona al menos una factura.") && length(eco()$guardado) == 0L && abierto()
  })
  ok(paste(et, "B9 Enviar: an amount typed and a row only ticked reach the Agenda; the modal closes"), {
    abrir(); abrir_grupos()
    escribe_en("F-101", "400")
    clic(casilla("F-201"))
    enviar()
    espera(function() length(eco()$guardado) == 2L) && {
      g <- eco()$guardado
      setequal(vapply(g, `[[`, "", "Documento"), c("F-101", "F-201")) &&
        identical(vapply(g, function(x) as.numeric(x$Importe), 0)[order(vapply(g, `[[`, "", "Documento"))], c(400, 800)) &&
        espera(function() !abierto()) && eco()$cierres == 1L
    }
  })
}

cat("\n=== results:", .pass, "passed,", .fail, "failed ===\n")
if (.fail > 0L) quit(status = 1L)
