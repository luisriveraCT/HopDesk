# =============================================================================
# tests/helpers/tema_comparar_estilos.R
# TEMA: compares two computed-style values (master vs branch) the way
# test_tema_clasico_igual_chromote.R needs. Pure functions, no I/O.
#
# A colour property (color, background-color, border-*-color, outline-color,
# text-decoration-color, fill, stroke) is read as RGBA whatever its format
# ("rgb(…)", "rgba(…)", "color(srgb r g b / a)", "transparent"), and two
# values count as the same when ΔE76 <= de_max and |alpha| <= alfa_max.
# box-shadow: its colours the same way, everything else (offsets, blur,
# spread, inset) exact. Any other property: exact.
#
#   tema_comparar_valor(prop, a, b, de_max, alfa_max)
#     -> list(estado = "igual" | "aviso" | "falla", de = <max ΔE or NA>)
#   "aviso": the same within tolerance but ΔE > 0 (Clásico moved a little).
# de_max comes from TEMA_DE_MAX (2 by default; 0 = strict). A different
# format with ΔE 0 is "igual" even when strict.
# =============================================================================

TEMA_PROPS_COLOR <- c("color", "background-color", "border-top-color", "border-right-color",
                      "border-bottom-color", "border-left-color", "outline-color",
                      "text-decoration-color", "fill", "stroke")

.TEMA_RE_COLOR <- "rgba?\\([^)]*\\)|color\\(srgb[^)]*\\)|\\btransparent\\b"

# "rgb(30, 58, 95)", "rgba(30, 58, 95, 0.07)", "color(srgb 0.11 0.22 0.37 / 0.07)",
# "transparent" -> c(r, g, b, a) with r, g, b in 0..255. NULL if it is not one.
tema_rgba <- function(x) {
  x <- trimws(tolower(x))
  if (identical(x, "transparent")) return(c(0, 0, 0, 0))
  if (grepl("^rgba?\\(", x)) {
    n <- as.numeric(strsplit(gsub("^rgba?\\(|\\)$", "", x), "[,/ ]+")[[1]])
    n <- n[!is.na(n)]
    if (length(n) == 3L) n <- c(n, 1)
    return(if (length(n) == 4L) n else NULL)
  }
  if (grepl("^color\\(srgb", x)) {
    partes <- strsplit(gsub("^color\\(srgb|\\)$", "", x), "/", fixed = TRUE)[[1]]
    n <- as.numeric(strsplit(trimws(partes[1]), "\\s+")[[1]])
    a <- if (length(partes) > 1L) as.numeric(trimws(partes[2])) else 1
    if (length(n) != 3L || anyNA(n) || is.na(a)) return(NULL)
    return(c(n * 255, a))
  }
  NULL
}

.tema_lab <- function(rgb) {
  v <- rgb / 255
  v <- ifelse(v <= 0.04045, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  xyz <- c(0.4124 * v[1] + 0.3576 * v[2] + 0.1805 * v[3],
           0.2126 * v[1] + 0.7152 * v[2] + 0.0722 * v[3],
           0.0193 * v[1] + 0.1192 * v[2] + 0.9505 * v[3]) / c(0.95047, 1, 1.08883)
  f <- ifelse(xyz > 216 / 24389, xyz^(1 / 3), (24389 / 27 * xyz + 16) / 116)
  c(116 * f[2] - 16, 500 * (f[1] - f[2]), 200 * (f[2] - f[3]))
}

# ΔE76 between two RGBA colours (channels rounded to 0..255 first: the
# browser keeps 8 bits). Two fully transparent colours are the same.
tema_de_rgba <- function(a, b) {
  if (a[4] == 0 && b[4] == 0) return(0)
  sqrt(sum((.tema_lab(round(a[1:3])) - .tema_lab(round(b[1:3])))^2))
}

.tema_comparar_colores <- function(ca, cb, de_max, alfa_max) {
  if (length(ca) != length(cb)) return(list(estado = "falla", de = NA_real_))
  de <- 0
  for (i in seq_along(ca)) {
    a <- tema_rgba(ca[i]); b <- tema_rgba(cb[i])
    if (is.null(a) || is.null(b)) {
      if (!identical(ca[i], cb[i])) return(list(estado = "falla", de = NA_real_))
      next
    }
    d <- tema_de_rgba(a, b)
    if (abs(a[4] - b[4]) > alfa_max + 1e-9 || d > de_max + 1e-9) return(list(estado = "falla", de = d))
    de <- max(de, d)
  }
  list(estado = if (de > 0.01) "aviso" else "igual", de = de)
}

tema_comparar_valor <- function(prop, a, b, de_max = 2, alfa_max = 0.01) {
  a <- a %||% ""; b <- b %||% ""
  if (identical(a, b)) return(list(estado = "igual", de = 0))
  if (prop %in% TEMA_PROPS_COLOR)
    return(.tema_comparar_colores(trimws(a), trimws(b), de_max, alfa_max))
  if (identical(prop, "box-shadow")) {
    ca <- regmatches(a, gregexpr(.TEMA_RE_COLOR, a, perl = TRUE))[[1]]
    cb <- regmatches(b, gregexpr(.TEMA_RE_COLOR, b, perl = TRUE))[[1]]
    resto <- function(x) gsub("\\s+", " ", trimws(gsub(.TEMA_RE_COLOR, "C", x, perl = TRUE)))
    if (!identical(resto(a), resto(b))) return(list(estado = "falla", de = NA_real_))
    return(.tema_comparar_colores(ca, cb, de_max, alfa_max))
  }
  list(estado = "falla", de = NA_real_)
}
