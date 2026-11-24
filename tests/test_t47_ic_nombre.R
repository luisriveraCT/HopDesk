# =============================================================================
# tests/test_t47_ic_nombre.R
# T47: apply_ic_filter()'s fallback for a client WITHOUT an intercompany
# registry (no CardCodes, no RFCs). Before T47 it compared the counterpart,
# upper-cased, with the companies' SHORT names exactly, so the ERP's legal name
# ("NETWORKS LOGISTICS SA DE CV") was never intercompany: measured in dev on
# 2026-11-17, 372 of 645 AP rows between the group's own companies passed as
# third-party flow. Now, given the session's empresas table, a row is IC when
# its RFC is a company's, or (without an RFC) its counterpart normalized is a
# company's razón social / short name / alias (R/empresas_identidad.R).
#
# What must hold:
#   1. the legal name with accents, punctuation and "S.A. de C.V." is IC;
#   2. nothing approximate: a name that only CONTAINS a company's is not;
#   3. the RFC decides when the row has one (confirms or vetoes);
#   4. a generic normalized name (one word under 5 letters) never marks alone;
#   5. without `empresas`, the output is identical() to the pre-T47
#      function; with a registry AND empresas (T90, which replaced
#      "donde hay registro, manda"): exactly the pre-T47 output minus the
#      rows the companies' identity calls IC -- the registry still decides
#      everything else, the identity only adds marks;
#   6. every caller passes the session's empresas.
#
# Run from project root:  Rscript tests/test_t47_ic_nombre.R
# =============================================================================
cat("=== test_t47_ic_nombre ===\n\n")
suppressPackageStartupMessages({ library(dplyr) })
`%||%` <- function(a, b) if (is.null(a)) b else a
for (e in parse("R/global.R"))
  if (is.call(e) && identical(e[[1]], as.name("<-")) && as.character(e[[2]])[1] %in% c("strip_accents"))
    eval(e, globalenv())
source("R/monedas.R")
normalize_code <- eval(parse(text = grep("^normalize_code <- ", readLines("R/persistence.R", warn = FALSE), value = TRUE)))
source("R/empresas_identidad.R")
suppressMessages(source("R/data_pipeline.R"))

.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) { message("  ERROR: ", e$message); FALSE })
  if (isTRUE(result)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else                { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

# The pre-T47 function, as it was (R/data_pipeline.R at 4554de3, messages
# left out), for identical().
antes <- function(df, mode, code_col, ic_codes, ic_rfcs = character(), cmap) {
  force(cmap)
  if (mode == "include") return(df)
  has_codes <- length(ic_codes) > 0
  has_rfcs  <- length(ic_rfcs) > 0 && "RFC" %in% names(df)
  if (!has_codes && !has_rfcs) {
    if ("Parte" %in% names(df) && length(cmap)) {
      is_ic_by_name <- toupper(trimws(df[["Parte"]])) %in% toupper(unname(cmap))
      if (mode == "exclude") return(df[!is_ic_by_name | is.na(is_ic_by_name), , drop = FALSE])
      else                   return(df[ is_ic_by_name & !is.na(is_ic_by_name), , drop = FALSE])
    }
    return(df)
  }
  if (has_codes && !is.null(code_col) && code_col %in% names(df)) {
    is_ic <- normalize_code(df[[code_col]]) %in% normalize_code(ic_codes)
  } else is_ic <- rep(FALSE, nrow(df))
  if (has_rfcs) {
    inv_rfc <- toupper(trimws(df[["RFC"]])); has_rfc <- nzchar(inv_rfc) & !is.na(inv_rfc)
    ic_rfcs_norm <- toupper(trimws(ic_rfcs[nzchar(ic_rfcs)]))
    is_ic[has_rfc &  (inv_rfc %in% ic_rfcs_norm)] <- TRUE
    is_ic[has_rfc & !(inv_rfc %in% ic_rfcs_norm)] <- FALSE
  }
  if (mode == "exclude") df[!is_ic | is.na(is_ic), , drop = FALSE] else df[is_ic & !is.na(is_ic), , drop = FALSE]
}

CMAP <- c(NL = "Networks Logistics", NTS = "Networks Transportes", NCS = "Networks Crossdocking Services", NG = "NG")
EMP <- data.frame(initials = c("NL", "NTS", "NCS", "NG"),
                  razon_social = c("Networks Logistics, S.A. de C.V.", "Networks Transportes SA de CV",
                                   "Networks Crossdocking Services S.A. de C.V.", "NG"),
                  nombre_corto = unname(CMAP), nombres_alt = c('["NWL"]', "[]", "[]", "[]"),
                  rfc = c("NLO010101AAA", "NTR020202BBB", "NCS030303CCC", NA),
                  activa = TRUE, deleted = FALSE, stringsAsFactors = FALSE)
df <- data.frame(
  Parte = c("NETWORKS LOGISTICS SA DE CV", "Networks Transportes, S.A. de C.V.", "NETWORKS LOGISTICS",
            "NETWORKS LOGISTICS INTERNATIONAL SA DE CV", "Aceros del Bajío SA de CV", "Otro nombre cualquiera",
            "NETWORKS CROSSDOCKING SERVICES SA DE CV", "NG"),
  `Código de proveedor` = paste0("P", 1:8),
  RFC = c(NA, "", NA, NA, NA, "NCS030303CCC", "XAXX010101000", NA),
  check.names = FALSE, stringsAsFactors = FALSE)
f <- function(empresas = EMP, mode = "exclude")
  suppressMessages(apply_ic_filter(df, mode = mode, code_col = "Código de proveedor", ic_codes = character(),
                                   ic_rfcs = character(), cmap = CMAP, empresas = empresas))$Parte
queda <- f()
previo <- function(mode = "exclude") antes(df, mode, "Código de proveedor", character(), character(), CMAP)

ok("1: la razón social del ERP ('NETWORKS LOGISTICS SA DE CV') es intercompañía", !"NETWORKS LOGISTICS SA DE CV" %in% queda)
ok("1b: con acentos, comas y 'S.A. de C.V.' también", !"Networks Transportes, S.A. de C.V." %in% queda)
ok("1c: el nombre corto exacto sigue siendo IC", !"NETWORKS LOGISTICS" %in% queda)
ok("2: nada aproximado: un nombre que solo CONTIENE el de la empresa no es IC; un tercero tampoco",
   all(c("NETWORKS LOGISTICS INTERNATIONAL SA DE CV", "Aceros del Bajío SA de CV") %in% queda))
ok("3: el RFC decide: un RFC de empresa es IC aunque el nombre no diga nada", !"Otro nombre cualquiera" %in% queda)
ok("3b: el RFC veta: mismo nombre que NCS, pero RFC de un tercero, no es IC",
   "NETWORKS CROSSDOCKING SERVICES SA DE CV" %in% queda)
ok("4: un nombre genérico (una palabra de menos de 5 letras, 'NG') nunca marca solo",
   "NG" %in% queda && !"NG" %in% empresas_identidad(EMP)$nombres$nombre)
ok("4b: 'only' devuelve exactamente las IC", setequal(f(mode = "only"),
   c("NETWORKS LOGISTICS SA DE CV", "Networks Transportes, S.A. de C.V.", "NETWORKS LOGISTICS", "Otro nombre cualquiera")))

cat("\n-- identical() con el filtro de antes --\n")
casos <- list(list(codes = "P5", rfcs = character()), list(codes = character(), rfcs = "NLO010101AAA"),
              list(codes = c("P1", "P2"), rfcs = "NTR020202BBB"))
ok("5: con registro y empresas (T90): exactamente lo de antes MENOS lo que la identidad llama IC; nada más cambia",
   all(vapply(casos, function(k) {
     a <- antes(df, "exclude", "Código de proveedor", k$codes, k$rfcs, CMAP)
     b <- suppressMessages(apply_ic_filter(df, "exclude", "Código de proveedor", k$codes, k$rfcs, CMAP, empresas = EMP))
     ident <- ic_por_identidad(a$Parte, if ("RFC" %in% names(a)) a$RFC else NULL, empresas_identidad(EMP))
     identical(b$Parte, a$Parte[!ident]) && identical(names(b), names(a))
   }, logical(1))))
ok("5b: sin empresas (NULL), identical() a la de antes, en exclude, only e include",
   all(vapply(c("exclude", "only", "include"), function(m)
     identical(previo(m), suppressMessages(apply_ic_filter(df, m, "Código de proveedor", character(), character(), CMAP))),
     logical(1))))
ok("5c: con empresas vacías o todas borradas, igual que antes",
   identical(f(empresas = EMP[0, ]), previo()$Parte) && identical(f(empresas = transform(EMP, deleted = TRUE)), previo()$Parte))
ok("5d: la detección de cuenta de Bancos usa el mismo normalizador, sin copias",
   !any(grepl("^\\.norm_razon <- |^\\.empresa_nombres <- |^\\.BNC_LEGAL_TOKENS <- ", readLines("R/bancos_detect.R"))) &&
     identical(.norm_razon("Networks Logistics, S.A. de C.V."), "NETWORKS LOGISTICS"))

cat("\n-- llamadores --\n")
# Every call in the parsed code (not text): an empty argument (df[x, ]) is
# checked inside the list, never bound to a variable.
llamadas_a <- function(archivo, fn) {
  out <- list()
  buscar <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1]], as.name(fn))) out[[length(out) + 1L]] <<- e
      l <- as.list(e)
      for (k in seq_along(l)[-1]) if (!(is.symbol(l[[k]]) && !nzchar(as.character(l[[k]])))) buscar(l[[k]])
    }
  }
  for (e in parse(archivo, keep.source = FALSE)) buscar(e)
  out
}
ok("6: cada llamada a apply_ic_filter en la app (8, en 6 archivos) pasa las empresas de la sesión", {
  arch <- c("R/ledger_module.R", "R/search_module.R", "R/vencidos_module.R", "R/cashflow_export_module.R",
            "R/avisos_umbrales.R", "R/avisos_datos.R")
  ll <- unlist(lapply(arch, llamadas_a, fn = "apply_ic_filter"), recursive = FALSE)
  length(ll) == 8L && all(vapply(ll, function(l) "empresas" %in% names(l), logical(1)))
})
ok("6b: ic_empresas_de(): las empresas de la sesión, o NULL si no hay (fixtures)",
   identical(ic_empresas_de(list(empresas_db = function() EMP)), EMP) && is.null(ic_empresas_de(list())) &&
     is.null(ic_empresas_de(list(empresas_db = function() stop("sin sesión")))))

cat(sprintf("\n%d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
