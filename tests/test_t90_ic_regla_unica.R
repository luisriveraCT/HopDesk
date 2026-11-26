# =============================================================================
# tests/test_t90_ic_regla_unica.R
# T90: one intercompany (IC) rule for every screen. apply_ic_filter() adds
# the companies' identity (T47: a group company's RFC, or without an RFC its
# razón social / short name / other names, normalized and exact) to the
# registry -- before T90 the identity counted only without a registry, and
# Vencidos had a name-only layer of its own.
#   - a name never beats an RFC: a third party with the same name and its
#     own RFC is not IC; without an RFC the name decides (as T47);
#   - it only ADDS marks: every row the registry calls IC still is;
#   - mode "only" shows exactly the rows "exclude" takes out;
#   - Panorama = Calendario = Vencidos (its real server) with a registry and
#     a group company nobody registered;
#   - 100k rows in reasonable time (normalized per distinct Parte/RFC).
#
# Run from project root:  Rscript tests/test_t90_ic_regla_unica.R
# =============================================================================
cat("=== test_t90_ic_regla_unica ===\n\n")
suppressPackageStartupMessages({ library(shiny); library(dplyr) })
.pass <- 0L; .fail <- 0L
ok <- function(label, expr) {
  r <- tryCatch(expr, error = function(e) { message("  ERROR: ", conditionMessage(e)); FALSE })
  if (isTRUE(r)) { cat(" PASS:", label, "\n"); .pass <<- .pass + 1L }
  else           { cat(" FAIL:", label, "\n"); .fail <<- .fail + 1L }
}

invisible(source("tests/helpers/vencidos_fixture_app.R", local = FALSE))   # Vencidos' real stack
.i18n_dir <- file.path(getwd(), "www", "i18n")
for (f in c("R/tier_registry.R", "R/permisos.R", "R/agenda_filas.R", "R/ui_components.R", "R/avisos.R",
            "R/avisos_alertas_module.R", "R/panorama_datos.R", "R/panorama_publicar_module.R", "R/panorama_module.R"))
  suppressMessages(source(f))
source("tests/helpers/ic_filtro_piezas.R")
for (e in parse("R/data_pipeline.R", keep.source = FALSE))
  if (is.call(e) && identical(as.character(e[[1]]), "<-") && identical(as.character(e[[2]]), "to_calendar_data"))
    eval(e, globalenv())

HOY <- hoy
REG <- list(ar_prefix = "C", ap_prefix = "P", companies = list(list(ar = "66", ap = "77")))
EMP <- data.frame(id = c("e1", "e2"), initials = c("NL", "NCS"),
                  nombre_corto = c("NETWORKS LOGISTICS", "NETWORKS CARGO"),
                  razon_social = c("NETWORKS LOGISTICS SA DE CV", "NETWORKS CARGO SERVICES SA DE CV"),
                  rfc = c("NLO010101AAA", "NCS020202BBB"), nombres_alt = NA_character_,
                  activa = TRUE, deleted = FALSE, stringsAsFactors = FALSE)
CMAP_T90 <- stats::setNames(EMP$nombre_corto, EMP$initials)
fila_ap <- function(doc, parte, rfc, codigo, saldo, dias = 0)
  data.frame(Empresa = "NTS", Moneda = "MXN", Documento = doc, Parte = parte, Factura = "", Codigo = codigo,
             FechaEff = HOY - dias, FechaVenc_Original = HOY - dias, `Saldo vencido` = saldo,
             source = SAP_B1_ERP_TYPE, confirmed = FALSE, id = "", provision_id = "",
             `Código de proveedor` = codigo, RFC = rfc, check.names = FALSE, stringsAsFactors = FALSE)
AP <- rbind(
  fila_ap("1", "ACEROS MONTERREY", "ACE010101AAA", "P10", 100),                  # third party: never IC
  fila_ap("2", "SOCIO REGISTRADO", "", "P77", 200),                               # registry code: IC before and after
  fila_ap("3", "NETWORKS CARGO", "", "P30", 300),                                 # short name, no RFC, not registered: IC now
  fila_ap("4", "Networks Logistics, S.A. de C.V.", "", "P31", 400),               # razón social, no RFC: IC now
  fila_ap("5", "OTRO NOMBRE", "NCS020202BBB", "P32", 500),                        # a group company's RFC: IC now
  fila_ap("6", "NETWORKS CARGO", "XYZ990101ZZZ", "P33", 600),                     # same name, third party's RFC: NOT IC
  fila_ap("7", "NETWORKS CARGO", "", "P34", 700))                                 # same name, no RFC: IC (the name decides)
ic_de <- function(df, empresas, modo = "exclude") {
  fuera <- apply_ic_filter(df, modo, "Código de proveedor", build_ic_fullcodes(REG, "AP"), character(),
                           cmap = CMAP_T90, empresas = empresas)
  if (modo == "exclude") setdiff(df$Documento, fuera$Documento) else fuera$Documento
}

cat("-- la regla --\n")
ic_rama <- ic_de(AP, EMP)
ok("IC con registro + identidad: la registrada (2), nombre corto (3), razón social (4), RFC del grupo (5), homónimo sin RFC (7)",
   setequal(ic_rama, c("2", "3", "4", "5", "7")))
ok("el nombre nunca le gana a un RFC: el homónimo con RFC de un tercero (6) no es IC; el tercero (1) tampoco",
   !any(c("1", "6") %in% ic_rama))
ok("solo agrega marcas: lo IC sin identidad (el master con registro) está contenido en lo IC con identidad",
   all(ic_de(AP, NULL) %in% ic_rama) && identical(ic_de(AP, NULL), "2"))
ok("el modo 'only' muestra exactamente lo que 'exclude' saca", setequal(ic_de(AP, EMP, "only"), ic_rama))
ok("sin registro ni empresas: el respaldo por nombre corto de siempre (sin cambio)", {
  fuera <- apply_ic_filter(AP, "exclude", "Código de proveedor", character(), character(), cmap = CMAP_T90, empresas = NULL)
  setequal(setdiff(AP$Documento, fuera$Documento), c("3", "6", "7"))
})
ok("con la regla del RFC en dos sentidos: un código registrado con RFC de un tercero que no está en la lista sigue sin ser IC (como antes)", {
  x <- fila_ap("8", "SOCIO", "TER010101TTT", "P77", 1)
  fuera <- apply_ic_filter(x, "exclude", "Código de proveedor", build_ic_fullcodes(REG, "AP"), "OTRO010101OOO",
                           cmap = CMAP_T90, empresas = EMP)
  nrow(fuera) == 1L
})

cat("\n-- Panorama = Calendario = Vencidos, con registro y empresas del grupo sin código --\n")
AR <- AP[0, ]
sh <- list(interco_v2 = function() REG, company_map = function() CMAP_T90, empresas_db = function() EMP,
           ic_mode = list(AR = function() "exclude", AP = function() "exclude"),
           df_combined_AR = function() AR, df_combined_AP = function() AP)
pan <- panorama_cobros_hoy(panorama_filas_vigentes(AP, "AP", sh), HOY)
cal <- local({
  d <- apply_ic_filter(AP, "exclude", "Código de proveedor", build_ic_fullcodes(REG, "AP"), character(),
                       cmap = CMAP_T90, empresas = EMP)
  cd <- to_calendar_data(d, "Saldo vencido"); sum(cd$Importe[as.Date(cd$Fecha) == HOY])
})
ven <- NULL
shv <- list(interco_v2 = reactive(REG), tags_db = reactive(tags0), pagar_hoy_db = reactive(agenda0),
            ic_mode = list(AR = reactive("exclude"), AP = reactive("exclude")),
            df_combined_AR = reactive(AR), df_combined_AP = reactive(AP), empresas_db = reactive(EMP),
            empresa_sel = reactive(c("NTS", "NCS")), company_map = reactive(CMAP_T90),
            current_user = reactive("luis"), sap_data = reactive(crudos0), amount_col = reactive("Saldo vencido"))
testServer(vencidosServer, args = list(shared = shv, accion_factura = function(...) NULL), {
  session$flushReact(); ven <<- vencidos_df()
})
ok("Panorama: solo 1 y 6 (100 + 600 = 700 MXN)", identical(sum(pan$total), 700) && identical(sum(pan$n), 2L))
ok("Calendario: lo mismo (700)", identical(cal, 700))
ok("Vencidos (su servidor real): las mismas facturas (1 y 6)", setequal(ven$Documento, c("1", "6")))
ok("Vencidos ya no tiene su capa por nombre", {
  x <- readLines("R/vencidos_module.R", warn = FALSE, encoding = "UTF-8")
  !any(grepl("is_ic_by_name", x, fixed = TRUE)) && !any(grepl("Layer 2", x, fixed = TRUE))
})

cat("\n-- tiempo: 100k filas --\n")
set.seed(90)
n <- 100000L
grande <- AP[rep(1:7, length.out = n), ]
grande$Parte <- paste(grande$Parte, sample(0:1999, n, TRUE) %/% 1000)   # ~14 distinct names
grande$Parte[seq(1, n, by = 9)] <- paste("PROVEEDOR", sample(1:3000, length(seq(1, n, by = 9)), TRUE))
grande$Documento <- as.character(seq_len(n))
# T114b: CPU time (user + sys), not wall clock: the work, not the wait for a
# loaded machine.
t_sin <- sum(system.time(apply_ic_filter(grande, "exclude", "Código de proveedor", build_ic_fullcodes(REG, "AP"), character(),
                                     cmap = CMAP_T90, empresas = NULL))[c("user.self", "sys.self")])
t_con <- sum(system.time(apply_ic_filter(grande, "exclude", "Código de proveedor", build_ic_fullcodes(REG, "AP"), character(),
                                     cmap = CMAP_T90, empresas = EMP))[c("user.self", "sys.self")])
cat(sprintf("   100k filas: sin identidad %.2f s, con identidad %.2f s\n", t_sin, t_con))
ok("con identidad, 100k filas en menos de 5 s (se normaliza por Parte/RFC distintos, no por fila)", t_con < 5)

cat(sprintf("\nTOTAL: %d passed, %d failed\n", .pass, .fail))
if (.fail > 0L) quit(status = 1L)
