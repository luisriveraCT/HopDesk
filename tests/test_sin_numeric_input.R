# =============================================================================
# tests/test_sin_numeric_input.R
# T66 ratchet: every number field is hd_numero_input() (R/ui_components.R),
# never Shiny's numericInput: a <input type="number"> changes with the mouse
# wheel and the arrow keys (Luis, "Sobretasa (bps)" in the Pasivos wizard).
#   - counts the calls to numericInput() and updateNumericInput() in R/ and
#     app.R (parsed code: comments and strings do not count);
#   - the count may only go DOWN: more than the ceiling fails, and fewer
#     fails too until the ceiling below is lowered to the new count;
#   - no <input type="number"> written by hand.
# The goal is 0 and 0.
# Run from the repo root:  Rscript tests/test_sin_numeric_input.R
# =============================================================================
library(testthat)

TOPE_NUMERIC_INPUT        <- 13L
TOPE_UPDATE_NUMERIC_INPUT <- 2L

archivos <- c(list.files("R", pattern = "\\.R$", recursive = TRUE, full.names = TRUE), "app.R")
archivos <- archivos[file.exists(archivos)]

tokens <- function(f) {
  pd <- utils::getParseData(parse(f, keep.source = TRUE), includeText = TRUE)
  pd[pd$terminal, c("line1", "token", "text")]
}
llamadas <- function(nombre) {
  out <- character()
  for (f in archivos) {
    t <- tokens(f)
    i <- which(t$token == "SYMBOL_FUNCTION_CALL" & t$text == nombre)
    if (length(i)) out <- c(out, paste0(f, ":", t$line1[i]))
  }
  out
}
detalle <- function(x) paste(x, collapse = "\n  ")

test_that("numericInput() solo puede bajar (meta 0)", {
  n <- llamadas("numericInput")
  if (length(n) > TOPE_NUMERIC_INPUT)
    fail(sprintf("Hay %d numericInput() y el tope es %d: usa hd_numero_input(). Están en:\n  %s",
                 length(n), TOPE_NUMERIC_INPUT, detalle(n)))
  if (length(n) < TOPE_NUMERIC_INPUT)
    fail(sprintf("Bajaron a %d numericInput(): baja TOPE_NUMERIC_INPUT a %d en este archivo.", length(n), length(n)))
  succeed()
})

test_that("updateNumericInput() solo puede bajar (meta 0)", {
  n <- llamadas("updateNumericInput")
  if (length(n) > TOPE_UPDATE_NUMERIC_INPUT)
    fail(sprintf("Hay %d updateNumericInput() y el tope es %d: usa hd_numero_update(). Están en:\n  %s",
                 length(n), TOPE_UPDATE_NUMERIC_INPUT, detalle(n)))
  if (length(n) < TOPE_UPDATE_NUMERIC_INPUT)
    fail(sprintf("Bajaron a %d updateNumericInput(): baja TOPE_UPDATE_NUMERIC_INPUT a %d en este archivo.", length(n), length(n)))
  succeed()
})

test_that("ningún <input type=\"number\"> escrito a mano", {
  malos <- character()
  for (f in archivos) {
    t <- tokens(f)
    # tags$input(type = "number"): the argument name, = and the string
    for (i in which(t$token == "SYMBOL_SUB" & t$text == "type")) {
      j <- i + 2L
      if (j <= nrow(t) && t$token[i + 1L] == "EQ_SUB" && t$token[j] == "STR_CONST" &&
          gsub("[\"']", "", t$text[j]) == "number") malos <- c(malos, paste0(f, ":", t$line1[i]))
    }
    # HTML written in a string
    s <- which(t$token == "STR_CONST" & grepl("type\\s*=\\s*\\\\?[\"']?number", t$text, ignore.case = TRUE))
    if (length(s)) malos <- c(malos, paste0(f, ":", t$line1[s]))
  }
  expect(!length(malos), sprintf("Campo type=\"number\" escrito a mano; usa hd_numero_input():\n  %s", detalle(malos)))
})

test_that("la guardiana ve una llamada nueva (contraprueba)", {
  tmp <- tempfile(fileext = ".R")
  writeLines(c("# numericInput( in a comment does not count",
               "x <- 'numericInput(\"a\")'",
               "y <- shiny::numericInput('b', NULL, 1)",
               "z <- tags$input(type = \"number\")"), tmp)
  t <- tokens(tmp)
  expect_equal(sum(t$token == "SYMBOL_FUNCTION_CALL" & t$text == "numericInput"), 1L)
  expect_true(any(t$token == "SYMBOL_SUB" & t$text == "type"))
})
