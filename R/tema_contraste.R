# =============================================================================
# R/tema_contraste.R — WCAG 2.x contrast between two colors (TEMA).
#
# Pure functions, base R only, nothing runs at load time (rule 29). Used by
# the theme guardian (tests/test_temas_guardian.R) to require AA on the text
# pairs of each theme, and by anything else that has to pick a readable color.
#
#   tema_luminancia("#13213A")            relative luminance, 0 (black) .. 1 (white)
#   tema_contraste("#777", "#FFF")        4.478  (always >= 1, order does not matter)
#   tema_pasa_aa("#777", "#FFF")          FALSE  (4.5 normal text, 3 large text)
#
# Colors are "#rgb" or "#rrggbb" (any case). A color with alpha is refused:
# its contrast depends on what lies beneath, so the caller must flatten it
# first. NA gives NA. Everything is vectorised (recycled like `+`).
# =============================================================================

# Relative luminance (WCAG 2.x, sRGB). The linearisation cut is 0.04045 (the
# sRGB standard; WCAG 2.0/2.1 printed 0.03928). For 8-bit channels both give
# the same result: no channel value falls between the two.
tema_luminancia <- function(color) {
  color <- as.character(color)
  out <- rep(NA_real_, length(color))
  ok <- !is.na(color)
  if (!any(ok)) return(out)
  hex <- sub("^#", "", trimws(color[ok]))
  malos <- !grepl("^([0-9A-Fa-f]{3}|[0-9A-Fa-f]{6})$", hex)
  if (any(malos))
    stop("tema_luminancia: not a #rgb / #rrggbb color: ",
         paste(unique(color[ok][malos]), collapse = ", "), call. = FALSE)
  corto <- nchar(hex) == 3L
  hex[corto] <- vapply(strsplit(hex[corto], ""), function(ch) paste0(rep(ch, each = 2L), collapse = ""), "")
  canal <- function(i) strtoi(substr(hex, i, i + 1L), 16L) / 255
  lin <- function(c) ifelse(c <= 0.04045, c / 12.92, ((c + 0.055) / 1.055)^2.4)
  out[ok] <- 0.2126 * lin(canal(1L)) + 0.7152 * lin(canal(3L)) + 0.0722 * lin(canal(5L))
  out
}

# Contrast ratio (L1 + 0.05) / (L2 + 0.05), L1 the lighter: 1 .. 21.
# Not rounded: compare the raw value against the threshold (4.4776 is not AA).
tema_contraste <- function(texto, fondo) {
  a <- tema_luminancia(texto)
  b <- tema_luminancia(fondo)
  (pmax(a, b) + 0.05) / (pmin(a, b) + 0.05)
}

# WCAG AA: 4.5 for normal text, 3 for large text (>= 24 px, or >= 18.66 px
# bold) and for non-text parts (borders of controls, focus rings, icons).
tema_pasa_aa <- function(texto, fondo, grande = FALSE) {
  tema_contraste(texto, fondo) >= ifelse(grande, 3, 4.5)
}
