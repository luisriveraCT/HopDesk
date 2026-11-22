# T79: the Usuarios permissions editor draws the permissions by group, with a
# title per group, grouped AFTER perm_visible_keys(). Run from the repo root:
#   Rscript tests/test_t79_permisos_grupos.R
# Checks, as staff and as a client, for every tier: the same keys and the same
# checkbox ids as before grouping (perm_visible_keys(), unlocked ones as
# inputs); a client never sees the "interno" title; no empty group is drawn;
# a key outside PERMISOS_GRUPOS falls into "sin_grupo" instead of disappearing;
# the weekly e-mail sits under "Panorama".
suppressPackageStartupMessages({ library(testthat); library(shiny) })
`%||%` <- function(a, b) if (is.null(a)) b else a
source("R/i18n.R")
source("R/tier_registry.R")
source("R/permisos.R")
# Only the file's function definitions (as test_editor_permisos_abre.R).
local({
  ex <- parse("R/tiers_module.R", keep.source = FALSE)
  es <- vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                 is.call(e[[3]]) && identical(e[[3]][[1]], as.name("function")), logical(1))
  for (e in ex[es]) eval(e, globalenv())
  stopifnot(exists(".tiers_perm_editor_ui"))
})
if (!exists(".tier_color")) .tier_color <- function(t) nivel_color(t)

ns  <- shiny::NS("t")
fila <- function(tier)
  data.frame(username = "x", tier = tier, permisos = "{}", allowed_clients = NA_character_,
             stringsAsFactors = FALSE)
html <- function(tier, staff)
  as.character(.tiers_perm_editor_ui(ns, fila(tier), viewer_tier = nivel_tope(),
                                     registry = NULL, viewer_is_staff = staff))
todos <- function(x, re) regmatches(x, gregexpr(re, x, perl = TRUE))[[1]]
# Group TITLES only (T79b: the "Ir a:" links carry data-grupo too).
RE_TITULO <- 'class="tiers-perm-grupo" data-grupo="[a-z_]+"'
RE_ENLACE <- 'class="tiers-ir-grupo" data-grupo="[a-z_]+"'
grupo_en    <- function(x) sub('.*data-grupo="([a-z_]+)".*', "\\1", x)
grupos_de   <- function(h) grupo_en(todos(h, RE_TITULO))
enlaces_de  <- function(h) grupo_en(todos(h, RE_ENLACE))
cajas_de    <- function(h) sub('^id="t-perm_', "", sub('"$', "", todos(h, 'id="t-perm_[a-z_]+"')))
# The group each row falls under: walk the HTML in order.
grupo_de_clave <- function(h, k) {
  pos_k <- regexpr(paste0('id="t-perm_', k, '"'), h, fixed = TRUE)
  if (pos_k < 0) return(NA_character_)
  m <- gregexpr(RE_TITULO, h, perl = TRUE)[[1]]
  antes <- m[m < pos_k]
  if (!length(antes)) return(NA_character_)
  grupo_en(regmatches(h, list(m))[[1]][length(antes)])
}

test_that("same keys and same checkbox ids as before grouping, as staff and as a client", {
  for (staff in c(TRUE, FALSE)) for (t in niveles_todos()) {
    h   <- html(t, staff)
    vis <- perm_visible_keys(staff)
    esperadas <- setdiff(vis, permisos_bloqueados(t))
    expect_setequal(cajas_de(h), esperadas)
    expect_identical(anyDuplicated(cajas_de(h)), 0L, info = t)
    # Every visible key's name is drawn once (locked ones without an input).
    for (k in vis) expect_true(grepl(permiso_nombre(k), h, fixed = TRUE), info = paste(t, k))
  }
})

test_that("a client never sees the internal group; staff does", {
  for (t in niveles_todos()) {
    expect_false("interno" %in% grupos_de(html(t, FALSE)), info = t)
    expect_false(grepl(tr("permisos.grupo.interno"), html(t, FALSE), fixed = TRUE), info = t)
  }
  expect_true("interno" %in% grupos_de(html(nivel_tope(), TRUE)))
})

test_that("groups follow PERMISOS_GRUPOS, each once, none empty", {
  for (staff in c(TRUE, FALSE)) {
    h <- html(nivel_tope(), staff)
    g <- grupos_de(h)
    expect_identical(anyDuplicated(g), 0L)
    expect_identical(g, intersect(PERMISOS_GRUPOS, g))
    con_claves <- unique(vapply(perm_visible_keys(staff), function(k) PERMISOS[[k]]$grupo, ""))
    expect_setequal(g, con_claves)
  }
})

test_that("the weekly e-mail sits under Panorama", {
  for (staff in c(TRUE, FALSE)) for (t in niveles_todos()) {
    if ("can_receive_weekly_email" %in% permisos_bloqueados(t)) next
    expect_identical(grupo_de_clave(html(t, staff), "can_receive_weekly_email"), "panorama",
                     info = paste(t, staff))
  }
})

test_that("a key outside PERMISOS_GRUPOS falls into 'sin_grupo' (Otros), last, and does not disappear", {
  viejo <- PERMISOS
  on.exit(PERMISOS <<- viejo, add = TRUE)
  PERMISOS[["can_prueba_t79"]] <<- list(grupo = "sin_grupo_t79")
  PERMISOS[["can_prueba_t79b"]] <<- list()
  for (staff in c(TRUE, FALSE)) {
    h <- html(nivel_tope(), staff)
    expect_identical(tail(grupos_de(h), 1), "sin_grupo")
    expect_identical(grupo_de_clave(h, "can_prueba_t79"), "sin_grupo")
    expect_identical(grupo_de_clave(h, "can_prueba_t79b"), "sin_grupo")
  }
})

test_that("T79b: one 'Ir a:' link per group title drawn, same order, each to an id that exists", {
  for (staff in c(TRUE, FALSE)) for (t in niveles_todos()) {
    h <- html(t, staff)
    expect_identical(enlaces_de(h), grupos_de(h), info = paste(t, staff))
    if (!staff) expect_false("interno" %in% enlaces_de(h), info = t)
    for (g in enlaces_de(h)) {
      id <- paste0("t-editor_permisos_grupo_", g)
      expect_true(grepl(sprintf('id="%s"', id), h, fixed = TRUE), info = g)
      # The onclick's quotes come out HTML-escaped (&#39;).
      expect_true(grepl(sprintf("getElementById(&#39;%s&#39;)?.scrollIntoView", id), h, fixed = TRUE), info = g)
      expect_true(grepl(tr(paste0("permisos.grupo_corto.", g)), h, fixed = TRUE), info = g)
    }
    # The new ids never take the checkboxes' prefix.
    expect_false(grepl('id="t-perm_editor', h, fixed = TRUE))
  }
})

test_that("T79b: an unknown group gets its 'Otros' link too", {
  viejo <- PERMISOS
  on.exit(PERMISOS <<- viejo, add = TRUE)
  PERMISOS[["can_prueba_t79"]] <<- list(grupo = "sin_grupo_t79")
  h <- html(nivel_tope(), FALSE)
  expect_identical(tail(enlaces_de(h), 1), "sin_grupo")
  expect_identical(enlaces_de(h), grupos_de(h))
})

test_that("the panel card keeps its id, and the badges are translated", {
  h <- html(nivel_tope(), TRUE)
  expect_true(grepl('id="t-editor_permisos_seccion"', h, fixed = TRUE))
  expect_true(grepl('id="t-editor_permisos_card"', h, fixed = TRUE))
  for (x in c("default (tier)", ">override<", "obligatorio — principal", "Modifícalo directamente"))
    expect_false(grepl(x, h, fixed = TRUE), info = x)
})
