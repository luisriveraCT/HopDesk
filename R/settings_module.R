# =============================================================================
# R/settings_module.R  —  Settings Hub
#
# Opened via the ⚙ gear icon in the calendar control bar (btn_settings).
#
# Sidebar panels:
#   1. Intercompany          — IC client / supplier code lists
#   2. Catálogo Proveedores  — vendor alias + CLABE for PPL export
#   3. Cuentas de Empresa    — bank accounts per company (feeds cuenta_origen)
#
# "Cuentas de Empresa" is a two-level hierarchy:
#   Banco (institution)  →  Cuenta (account at that bank)
#
#   Banco fields:  nombre, clave (3-digit CECOBAN)
#   Cuenta fields: Empresa, razon_social, rfc, Moneda, alias (≤15, informational),
#                  cuenta (12-digit number used as PPL origen field),
#                  clabe_interbancaria (18-digit CLABE, for reference),
#                  is_ppl_default (TRUE = use this account as PPL cuenta origen),
#                  banco_id (FK), activa
#
# ── Integration changes needed in app.R ──────────────────────────────────────
# global.R — add to S3_KEYS:
#   ctas_bancos  = "ctas_bancos.rds"
#   ctas_cuentas = "ctas_cuentas.rds"
#
# app.R server — add reactiveVals:
#   ctas_cuentas <- reactiveVal(load_ctas_cuentas())
#   # add to shared list: ctas_cuentas = ctas_cuentas
#
# app.R server — add alongside other settings_* calls:
#   settings_cuentas_observer(input, output, session, shared)
#
# pagar_hoy_module.R — replace .get_cuenta_origen() calls with:
#   get_cuenta_ppl(empresa, moneda, shared)
# =============================================================================

# ── Public helper: fetch 12-digit origin account for PPL ──────────────────────
get_cuenta_ppl <- function(empresa, moneda, shared) {
  ctas <- tryCatch({
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas()
    else load_ctas_cuentas()
  }, error = function(e) NULL)

  if (is.null(ctas) || !nrow(ctas)) {
    return(.get_cuenta_origen_legacy(empresa, moneda, shared))
  }

  # ctas$Empresa stores initials ("NTS"); callers may pass either the full name
  # ("Networks Trucking Services") or initials. Accept both.
  ini          <- names(COMPANY_MAP)[unname(COMPANY_MAP) == empresa]
  empresa_keys <- unique(c(empresa, ini))

  # Priority 1: empresa + moneda + PPL default flag
  m <- ctas |> dplyr::filter(
    Empresa %in% empresa_keys, toupper(Moneda) == toupper(moneda),
    activa == TRUE, isTRUE(is_ppl_default)) |> dplyr::slice(1)
  if (nrow(m) && nzchar(trimws(m$cuenta[1]))) return(trimws(m$cuenta[1]))

  # Priority 2: empresa + moneda, any active
  m <- ctas |> dplyr::filter(
    Empresa %in% empresa_keys, toupper(Moneda) == toupper(moneda),
    activa == TRUE) |> dplyr::slice(1)
  if (nrow(m) && nzchar(trimws(m$cuenta[1]))) return(trimws(m$cuenta[1]))

  # Priority 3: empresa only (currency-agnostic fallback)
  m <- ctas |> dplyr::filter(Empresa %in% empresa_keys, activa == TRUE) |> dplyr::slice(1)
  if (nrow(m) && nzchar(trimws(m$cuenta[1]))) return(trimws(m$cuenta[1]))

  ""
}

.get_cuenta_origen_legacy <- function(empresa, moneda, shared) {
  if (!is.null(shared$bancos_cuentas)) {
    ctas <- tryCatch(shared$bancos_cuentas(), error = function(e) NULL)
    if (!is.null(ctas) && nrow(ctas)) {
      m <- ctas |> dplyr::filter(Empresa == empresa, activa == TRUE) |> dplyr::slice(1)
      if (nrow(m) && nzchar(trimws(m$cuenta[1]))) return(trimws(m$cuenta[1]))
    }
  }
  legacy <- tryCatch(load_bancos(), error = function(e) NULL)
  if (!is.null(legacy) && nrow(legacy)) {
    m <- legacy |> dplyr::filter(Empresa == empresa, activa == TRUE) |> dplyr::slice(1)
    if (nrow(m) && nzchar(trimws(m$cuenta[1]))) return(trimws(m$cuenta[1]))
  }
  ""
}

# ── Schemas ────────────────────────────────────────────────────────────────────
.schema_ctas_bancos <- function() tibble::tibble(
  id     = character(),
  nombre = character(),
  clave  = character()   # 3-digit CECOBAN code, e.g. "030"
)

.schema_ctas_cuentas <- function() tibble::tibble(
  id                  = character(),
  banco_id            = character(),   # FK → ctas_bancos$id
  Empresa             = character(),   # initials: "NTS", "NCS", …
  razon_social        = character(),
  rfc                 = character(),
  Moneda              = character(),   # "MXN" | "USD"
  alias               = character(),   # informational (≤15 chars)
  cuenta              = character(),   # 11-digit account used as PPL origen
  clabe_interbancaria = character(),   # 18-digit CLABE for reference / SPEI receipts
  is_ppl_default      = logical(),
  saldo_inicial       = numeric(),
  activa              = logical()
)

load_ctas_bancos <- function() {
  obj <- tryCatch(.s3_read(S3_KEYS$ctas_bancos), error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj)) return(.schema_ctas_bancos())
  schema <- .schema_ctas_bancos()
  for (col in names(schema))
    if (!col %in% names(obj)) obj[[col]] <- schema[[col]][NA_integer_]
  obj
}

load_ctas_cuentas <- function() {
  obj <- tryCatch(.s3_read(S3_KEYS$ctas_cuentas), error = function(e) NULL)
  if (is.null(obj) || !is.data.frame(obj)) return(.schema_ctas_cuentas())
  schema <- .schema_ctas_cuentas()
  for (col in names(schema))
    if (!col %in% names(obj)) obj[[col]] <- schema[[col]][NA_integer_]
  obj
}

save_ctas_bancos  <- function(df) .s3_write(df, S3_KEYS$ctas_bancos)
save_ctas_cuentas <- function(df) .s3_write(df, S3_KEYS$ctas_cuentas)

# ── Complete MX bank catalog (BanBajío CECOBAN codes) ─────────────────────────
# Format: "Nombre|CLAVE" — JS splits on "|" to fill the two text inputs.
# Source: BanBajío Catálogos BB (89 institutions).
.BANCOS_MX_CHOICES <- c(
  "— escribir manualmente —"       = "",
  "BanBajío (030)"                 = "BanBajío|030",
  "BBVA Bancomer (012)"            = "BBVA Bancomer|012",
  "Banamex / Citibanamex (002)"    = "Banamex|002",
  "Santander (014)"                = "Santander|014",
  "Banorte / IXE (072)"            = "Banorte|072",
  "HSBC (021)"                     = "HSBC|021",
  "Scotiabank (044)"               = "Scotiabank|044",
  "Inbursa (036)"                  = "Inbursa|036",
  "Banregio (058)"                 = "Banregio|058",
  "Afirme (062)"                   = "Afirme|062",
  "ABC Capital (138)"              = "ABC Capital|138",
  "Actinver (133)"                 = "Actinver|133",
  "Arcus (706)"                    = "Arcus|706",
  "ASP Integra OPC (659)"          = "ASP Integra OPC|659",
  "Autofin (128)"                  = "Autofin|128",
  "Azteca (127)"                   = "Azteca|127",
  "Babien (166)"                   = "Babien|166",
  "Banco Covalto (154)"            = "Banco Covalto|154",
  "Banco S3 (160)"                 = "Banco S3|160",
  "Bancomext (006)"                = "Bancomext|006",
  "Bancoppel (137)"                = "Bancoppel|137",
  "Bancrea (152)"                  = "Bancrea|152",
  "Banjercito (019)"               = "Banjercito|019",
  "Bank of America (106)"          = "Bank of America|106",
  "Bank of China (159)"            = "Bank of China|159",
  "Bankaool (147)"                 = "Bankaool|147",
  "Banobras (009)"                 = "Banobras|009",
  "Bansi (060)"                    = "Bansi|060",
  "Barclays (129)"                 = "Barclays|129",
  "Bbase (145)"                    = "Bbase|145",
  "Bmonex (112)"                   = "Bmonex|112",
  "Caja Pop Mexicana (677)"        = "Caja Pop Mexicana|677",
  "Caja Telefonista (683)"         = "Caja Telefonista|683",
  "CB Intercam (630)"              = "CB Intercam|630",
  "CBM Banco (124)"                = "CBM Banco|124",
  "CI Bolsa (631)"                 = "CI Bolsa|631",
  "CIBanco (143)"                  = "CIBanco|143",
  "Compartamos (130)"              = "Compartamos|130",
  "Consubanco (140)"               = "Consubanco|140",
  "Credicapital (652)"             = "Credicapital|652",
  "Crediclub (688)"                = "Crediclub|688",
  "Credit Suisse (126)"            = "Credit Suisse|126",
  "Cristobal Colon (680)"          = "Cristobal Colon|680",
  "Cuenca (723)"                   = "Cuenca|723",
  "Donde (151)"                    = "Donde|151",
  "Finamex (616)"                  = "Finamex|616",
  "Fincomun (634)"                 = "Fincomun|634",
  "Fomped (689)"                   = "Fomped|689",
  "Fondeadora (699)"               = "Fondeadora|699",
  "Fondo FIRA (685)"               = "Fondo FIRA|685",
  "GBM (601)"                      = "GBM|601",
  "HDI Seguros (636)"              = "HDI Seguros|636",
  "Hipotecaria Federal (168)"      = "Hipotecaria Federal|168",
  "ICBC (155)"                     = "ICBC|155",
  "Indeval (902)"                  = "Indeval|902",
  "Inmobiliario (150)"             = "Inmobiliario|150",
  "Intercam Banco (136)"           = "Intercam Banco|136",
  "Invercap (686)"                 = "Invercap|686",
  "Invex (059)"                    = "Invex|059",
  "JP Morgan (110)"                = "JP Morgan|110",
  "Klar (661)"                     = "Klar|661",
  "Kuspit (653)"                   = "Kuspit|653",
  "Libertad (670)"                 = "Libertad|670",
  "Masari (602)"                   = "Masari|602",
  "Mercado Pago W (722)"           = "Mercado Pago W|722",
  "Mifel (042)"                    = "Mifel|042",
  "Mizuho Bank (158)"              = "Mizuho Bank|158",
  "Monexcb (600)"                  = "Monexcb|600",
  "MUFG (108)"                     = "MUFG|108",
  "Multiva Banco (132)"            = "Multiva Banco|132",
  "Multiva Cbolsa (613)"           = "Multiva Cbolsa|613",
  "NAFIN (135)"                    = "NAFIN|135",
  "Nu Mexico (638)"                = "Nu Mexico|638",
  "NVIO (710)"                     = "NVIO|710",
  "Oskndia (649)"                  = "Oskndia|649",
  "Pagatodo (148)"                 = "Pagatodo|148",
  "Profuturo (620)"                = "Profuturo|620",
  "Sabadell (156)"                 = "Sabadell|156",
  "Shinhan (157)"                  = "Shinhan|157",
  "Skandia (623)"                  = "Skandia|623",
  "STP (646)"                      = "STP|646",
  "Tactiv CB (648)"                = "Tactiv CB|648",
  "Tesored (703)"                  = "Tesored|703",
  "Transfer (684)"                 = "Transfer|684",
  "Unagra (656)"                   = "Unagra|656",
  "Valmex (617)"                   = "Valmex|617",
  "Value (605)"                    = "Value|605",
  "Ve Por Mas (113)"               = "Ve Por Mas|113",
  "Vector (608)"                   = "Vector|608",
  "Volkswagen (141)"               = "Volkswagen|141",
  "Otro"                           = "Otro|"
)

# =============================================================================
# show_settings_modal
# =============================================================================
show_settings_modal <- function(input, output, session, shared) {

  # Resolve tier before building the modal — must happen outside tagList/div
  is_dev <- identical(
    tryCatch(shared$current_user_info()$tier, error = function(e) ""),
    "dev"
  )

  usuarios_btn <- if (is_dev) {
    tagList(
      tags$hr(class = "my-1"),
      tags$p(class = "text-muted small px-2 mb-1 text-uppercase fw-semibold",
             "Admin"),
      actionButton("stg_btn_usuarios",
        tagList(icon("users"), " Usuarios"),
        class = "btn btn-sm btn-outline-secondary text-start w-100")
    )
  } else NULL

  showModal(modalDialog(
    title     = tagList(icon("gear"), " Configuración"),
    size      = "xl",
    easyClose = FALSE,
    footer    = actionButton("stg_close_btn",
                             tagList(icon("xmark"), " Cerrar"),
                             class = "btn btn-secondary"),

    div(class = "settings-hub d-flex", style = "min-height: 460px;",

      # ── Sidebar ──────────────────────────────────────────────────────────
      div(class = "settings-sidebar d-flex flex-column gap-1 p-2 border-end bg-light flex-shrink-0",
        style = "width: 215px;",
        tags$p(class = "text-muted small px-2 pt-1 mb-1 text-uppercase fw-semibold",
               "Módulos"),
        actionButton("stg_btn_interco",
          tagList(icon("arrows-left-right"), " Intercompany"),
          class = "btn btn-sm btn-outline-secondary text-start w-100"),
        actionButton("stg_btn_catalogo",
          tagList(icon("building"), " Business Partners"),
          class = "btn btn-sm btn-outline-secondary text-start w-100"),
        actionButton("stg_btn_cuentas",
          tagList(icon("building-columns"), " Cuentas de Empresa"),
          class = "btn btn-sm btn-outline-secondary text-start w-100"),
        usuarios_btn
      ),

      # ── Content ──────────────────────────────────────────────────────────
      div(class = "settings-content flex-grow-1 p-3 overflow-auto",
        uiOutput("settings_panel")
      )
    )
  ))

  output$settings_panel <- renderUI({ .settings_welcome_ui() })
}

.settings_welcome_ui <- function() {
  div(class = "text-center text-muted pt-5",
    icon("gear", style = "font-size:2.5rem; opacity:0.2;"),
    tags$p(class = "mt-3", "Selecciona una sección en el menú de la izquierda.")
  )
}

# =============================================================================
# settings_observers  (called once from app.R server)
# =============================================================================
settings_observers <- function(input, output, session, shared) {

  # ── Intercompany ─────────────────────────────────────────────────────────────

  # ic_trigger: increment to re-render all company/ledger code lists from saved state.
  # renderUI outputs depend ONLY on this — nothing else — so they never fire
  # mid-edit and overwrite what the user is typing.
  ic_trigger      <- reactiveVal(0L)
  # Number of visible input slots per company/ledger ("{initials}_ar" / "_ap")
  ic_slots        <- reactiveValues()
  # TRUE while the IC panel is currently displayed
  ic_panel_active <- reactiveVal(FALSE)
  # Stores the pending nav target when a dirty-guard modal is shown
  ic_pending_nav  <- reactiveVal(NULL)
  # Business Partners: cached scan of unique SAP codes from invoice snapshots
  bp_candidates        <- reactiveVal(NULL)
  # Sorted version of bp_candidates (matches DT display order for row-index alignment)
  bp_sorted_candidates <- reactiveVal(NULL)


  # Auto-grow: when the current last slot is non-empty, append a new empty one
  # via insertUI so existing inputs are never touched or re-rendered.
  observe({
    cmap <- shared$company_map()   # reactive dep — re-fires when companies change
    lapply(names(cmap), function(initials) {
      local({
        ini <- initials
        lapply(c("ar", "ap"), function(ledger) {
          local({
            l   <- ledger
            key <- paste0(ini, "_", l)
            n   <- ic_slots[[key]] %||% 1L
            val <- input[[paste0("ic_", l, "_", ini, "_", n)]] %||% ""
            if (nzchar(trimws(val))) {
              new_n <- n + 1L
              ic_slots[[key]] <- new_n
              insertUI(
                selector  = paste0("#ic_", l, "_container_", ini),
                where     = "beforeEnd",
                immediate = TRUE,
                ui = div(
                  id = paste0("ic_", l, "_slot_", ini, "_", new_n),
                  class = "mb-2",
                  textInput(
                    inputId = paste0("ic_", l, "_", ini, "_", new_n),
                    label   = NULL, value = "", placeholder = "", width = "100%"
                  ),
                  tags$small(
                    id    = paste0("ic_lbl_", l, "_", ini, "_", new_n),
                    class = "d-block fst-italic",
                    style = "margin-top:-10px; font-size:0.7em; min-height:0.85em; padding-left:3px; color:#aaa;",
                    ""
                  )
                )
              )
            }
          })
        })
      })
    })
  })

  # ── Unsaved-changes helpers ───────────────────────────────────────────────────

  # Snapshot of current UI state (codes stripped of any accidental prefix, sorted)
  .ic_current <- function() {
    cmap <- shared$company_map()
    list(
      ar_prefix = trimws(input$ic_ar_prefix %||% "C"),
      ap_prefix = trimws(input$ic_ap_prefix %||% "P"),
      companies = setNames(lapply(names(cmap), function(ini) {
        ar_n <- isolate(ic_slots[[paste0(ini, "_ar")]]) %||% 1L
        ap_n <- isolate(ic_slots[[paste0(ini, "_ap")]]) %||% 1L
        list(
          ar = sort(unique(Filter(nzchar, sub("^[A-Za-z]+", "", trimws(
            vapply(seq_len(ar_n), function(j)
              input[[paste0("ic_ar_", ini, "_", j)]] %||% "", character(1))))))),
          ap = sort(unique(Filter(nzchar, sub("^[A-Za-z]+", "", trimws(
            vapply(seq_len(ap_n), function(j)
              input[[paste0("ic_ap_", ini, "_", j)]] %||% "", character(1)))))))
        )
      }), names(cmap))
    )
  }

  .ic_saved <- function() {
    cmap <- shared$company_map()
    cur  <- shared$interco_v2()
    list(
      ar_prefix = cur$ar_prefix %||% "C",
      ap_prefix = cur$ap_prefix %||% "P",
      companies = setNames(lapply(names(cmap), function(ini) {
        co <- cur$companies[[ini]] %||% list(ar = character(), ap = character())
        list(ar = sort(co$ar %||% character()), ap = sort(co$ap %||% character()))
      }), names(cmap))
    )
  }

  .ic_has_changes <- function() ic_panel_active() && !identical(.ic_current(), .ic_saved())

  .ic_unsaved_modal <- function() {
    modalDialog(
      title     = tagList(icon("triangle-exclamation"), " Cambios sin guardar"),
      p("Tienes cambios sin guardar en la configuraci\u00f3n Intercompany."),
      p(strong("\u00bfDeseas salir sin guardar?")),
      footer    = tagList(
        actionButton("ic_discard_confirm", "Salir sin guardar",
                     class = "btn btn-warning btn-sm"),
        tags$span("\u00a0"),
        actionButton("ic_discard_cancel",  "Seguir editando",
                     class = "btn btn-primary btn-sm")
      ),
      easyClose = FALSE
    )
  }

  # Execute a deferred nav after the user confirms discarding changes.
  # Note: showModal(.ic_unsaved_modal()) replaced the settings modal, so after
  # removeModal() the settings container is gone.  For non-close actions we
  # must re-open it before rendering the target panel.
  observeEvent(input$ic_discard_confirm, {
    removeModal()          # close the warning dialog
    action <- ic_pending_nav()
    ic_pending_nav(NULL)
    ic_panel_active(FALSE)

    if (action == "close") {
      # Settings modal already gone — nothing more to do
    } else if (action == "catalogo") {
      show_settings_modal(input, output, session, shared)
      output$settings_panel <- renderUI({
        .bp_panel_ui(shared, bp_candidates())
      })
    } else if (action == "cuentas") {
      show_settings_modal(input, output, session, shared)
      output$settings_panel <- renderUI({ .cuentas_panel_ui(cmap = shared$company_map()) })
    }
  })

  observeEvent(input$ic_discard_cancel, {
    removeModal()
    ic_pending_nav(NULL)
  })

  # Settings modal close button (replaces modalButton so we can guard it)
  observeEvent(input$stg_close_btn, {
    if (.ic_has_changes()) {
      ic_pending_nav("close")
      showModal(.ic_unsaved_modal())
    } else {
      ic_panel_active(FALSE)
      removeModal()
    }
  })

  # ── Open Intercompany panel ───────────────────────────────────────────────────
  # Helper: initialize ic_slots from registry and render the IC settings panel.
  # Call whenever navigating to IC panel (button click or post-scanner-apply).
  .show_ic_panel <- function() {
    cmap <- shared$company_map()
    cur  <- shared$interco_v2()
    ic_panel_active(TRUE)
    for (ini in names(cmap)) {
      co <- cur$companies[[ini]] %||% list(ar = character(), ap = character())
      ic_slots[[paste0(ini, "_ar")]] <- max(1L, length(co$ar %||% character()) + 1L)
      ic_slots[[paste0(ini, "_ap")]] <- max(1L, length(co$ap %||% character()) + 1L)
    }
    # Build + send code→name lookup so labels render correctly
    lkp <- tryCatch(.build_code_lookup(), error = function(e) list())
    ic_code_lookup(lkp)
    session$sendCustomMessage("icLookupData", jsonlite::toJSON(lkp, auto_unbox = TRUE))
    ic_trigger(ic_trigger() + 1L)

    output$settings_panel <- renderUI({
      ic_trigger()   # reactive dependency: re-render whenever codes are applied/loaded
      cmap <- shared$company_map()   # reactive dep — re-renders when companies change
      cur2 <- isolate(shared$interco_v2())
      lkp  <- isolate(ic_code_lookup())

      .slots_ui <- function(ledger, ini) {
        n        <- isolate(ic_slots[[paste0(ini, "_", ledger)]]) %||% 1L
        existing <- (cur2$companies[[ini]] %||%
                       list(ar = character(), ap = character()))[[ledger]] %||% character()
        ph <- if (ledger == "ar") "Ej: 1027" else "Ej: 1426"
        div(
          id = paste0("ic_", ledger, "_container_", ini),
          tagList(lapply(seq_len(n), function(j) {
            val      <- if (j <= length(existing)) existing[[j]] else ""
            lbl_text <- if (nzchar(val) && !is.null(lkp[[ledger]][[ini]])) {
              lkp[[ledger]][[ini]][[paste0("C", val)]] %||%
              lkp[[ledger]][[ini]][[paste0("P", val)]] %||%
              lkp[[ledger]][[ini]][[val]]              %||% ""
            } else ""
            div(id = paste0("ic_", ledger, "_slot_", ini, "_", j), class = "mb-2",
              textInput(
                inputId     = paste0("ic_", ledger, "_", ini, "_", j),
                label       = NULL,
                value       = val,
                placeholder = if (j == 1) ph else "",
                width       = "100%"
              ),
              tags$small(
                id    = paste0("ic_lbl_", ledger, "_", ini, "_", j),
                class = "d-block fst-italic",
                style = paste0("margin-top:-10px; font-size:0.7em; min-height:0.85em;",
                               " padding-left:3px;",
                               if (nzchar(lbl_text)) " color:#0d6efd;" else " color:#aaa;"),
                lbl_text
              )
            )
          }))
        )
      }

      panels <- lapply(names(cmap), function(initials) {
        ini <- initials
        bslib::accordion_panel(
          title = paste0(cmap[[initials]], " (", initials, ")"),
          value = initials,
          fluidRow(
            column(6,
              tags$label("Clientes IC \u2014 CxC", class = "fw-semibold small mb-1 d-block"),
              tags$p(class = "text-muted small mb-2",
                     "Un c\u00f3digo por caja. Al escribir aparece otra autom\u00e1ticamente."),
              .slots_ui("ar", ini)
            ),
            column(6,
              tags$label("Proveedores IC \u2014 CxP", class = "fw-semibold small mb-1 d-block"),
              tags$p(class = "text-muted small mb-2",
                     "Un c\u00f3digo por caja. Al escribir aparece otra autom\u00e1ticamente."),
              .slots_ui("ap", ini)
            )
          )
        )
      })

      # ── Registry summary card ──────────────────────────────────────────────
      ar_pre  <- cur2$ar_prefix %||% "C"
      ap_pre  <- cur2$ap_prefix %||% "P"
      reg_rows <- Filter(Negate(is.null), lapply(names(cmap), function(ini) {
        co    <- cur2$companies[[ini]] %||% list(ar = character(), ap = character())
        ar_cs <- co$ar %||% character()
        ap_cs <- co$ap %||% character()
        if (!length(ar_cs) && !length(ap_cs)) return(NULL)
        tags$tr(
          tags$td(tags$code(ini)),
          tags$td(class = "small font-monospace",
                  if (length(ar_cs)) paste(paste0(ar_pre, ar_cs), collapse = ", ")
                  else tags$span(class = "text-muted", "\u2014")),
          tags$td(class = "small font-monospace",
                  if (length(ap_cs)) paste(paste0(ap_pre, ap_cs), collapse = ", ")
                  else tags$span(class = "text-muted", "\u2014"))
        )
      }))
      total_ar <- sum(vapply(cur2$companies %||% list(),
                             function(co) length(co$ar %||% character()), integer(1)))
      total_ap <- sum(vapply(cur2$companies %||% list(),
                             function(co) length(co$ap %||% character()), integer(1)))

      summary_card <- if (length(reg_rows)) {
        div(class = "border rounded p-3 mb-3 border-success bg-success-subtle",
          tags$p(class = "fw-semibold small mb-2 text-success-emphasis",
                 tagList(icon("circle-check"),
                         sprintf(" Registro guardado \u2014 %d c\u00f3digo(s) CxC \u00b7 %d c\u00f3digo(s) CxP",
                                 total_ar, total_ap))),
          tags$div(class = "overflow-auto", style = "max-height:140px;",
            tags$table(class = "table table-sm table-borderless mb-0",
              tags$thead(tags$tr(
                tags$th(class = "small", "Empresa"),
                tags$th(class = "small", "Clientes IC (CxC)"),
                tags$th(class = "small", "Proveedores IC (CxP)")
              )),
              tags$tbody(reg_rows)
            )
          )
        )
      } else {
        div(class = "border rounded p-3 mb-3 text-muted small",
            tagList(icon("circle-info"), " Sin c\u00f3digos IC guardados todav\u00eda."))
      }

      div(
        tags$h6(class = "fw-semibold mb-3",
                tagList(icon("arrows-left-right"), " Configurar Intercompany")),
        summary_card,
        div(
          class = "border rounded p-3 mb-3 bg-light",
          tags$p(class = "fw-semibold small mb-2", "Prefijos de c\u00f3digos SAP",
                 tags$span(class = "text-muted fw-normal ms-2 fst-italic",
                           "(modificar solo si tu SAP usa otra convenci\u00f3n)")),
          fluidRow(
            column(3, textInput("ic_ar_prefix", "Prefijo CxC",
                                value = cur2$ar_prefix %||% "C", width = "70px")),
            column(3, textInput("ic_ap_prefix", "Prefijo CxP",
                                value = cur2$ap_prefix %||% "P", width = "70px"))
          )
        ),
        do.call(bslib::accordion,
          c(list(id = "ic_empresa_accordion", open = FALSE), panels)
        ),
        div(class = "mt-3 d-flex align-items-center gap-2 flex-wrap",
          actionButton("save_interco_v2", tagList(icon("floppy-disk"), " Guardar"),
                       class = "btn btn-primary btn-sm"),
          if (isTRUE(isolate(shared$current_user_info())$tier == "dev"))
            tagList(
              tags$span(class = "text-muted", "|"),
              actionButton("scan_ic_btn",
                           tagList(icon("magnifying-glass"), " Esc\u00e1ner SAP"),
                           class = "btn btn-outline-secondary btn-sm"),
              tags$span(class = "text-muted small",
                        "Detecta c\u00f3digos IC desde las facturas cargadas en memoria.")
            )
        )
      )
    })
  }

  observeEvent(input$stg_btn_interco, {
    .show_ic_panel()
  }, ignoreInit = TRUE)

  # ── Save ─────────────────────────────────────────────────────────────────────
  observeEvent(input$save_interco_v2, {
    cmap <- isolate(shared$company_map())

    parse_code <- function(val) {
      v        <- trimws(val %||% "")
      stripped <- sub("^[A-Za-z]+", "", v)
      if (nzchar(stripped)) stripped else character(0)
    }

    had_prefix <- any(vapply(names(cmap), function(initials) {
      ar_n <- ic_slots[[paste0(initials, "_ar")]] %||% 1L
      ap_n <- ic_slots[[paste0(initials, "_ap")]] %||% 1L
      any(grepl("^[A-Za-z]", c(
        vapply(seq_len(ar_n), function(j)
          input[[paste0("ic_ar_", initials, "_", j)]] %||% "", character(1)),
        vapply(seq_len(ap_n), function(j)
          input[[paste0("ic_ap_", initials, "_", j)]] %||% "", character(1))
      )))
    }, logical(1)))

    ar_prefix <- trimws(input$ic_ar_prefix %||% "C")
    ap_prefix <- trimws(input$ic_ap_prefix %||% "P")

    companies <- setNames(
      lapply(names(cmap), function(initials) {
        ar_n <- ic_slots[[paste0(initials, "_ar")]] %||% 1L
        ap_n <- ic_slots[[paste0(initials, "_ap")]] %||% 1L
        list(
          ar = unique(unlist(lapply(seq_len(ar_n), function(j)
            parse_code(input[[paste0("ic_ar_", initials, "_", j)]])))),
          ap = unique(unlist(lapply(seq_len(ap_n), function(j)
            parse_code(input[[paste0("ic_ap_", initials, "_", j)]]))))
        )
      }),
      names(cmap)
    )

    registry <- list(
      ar_prefix = if (nzchar(ar_prefix)) ar_prefix else "C",
      ap_prefix = if (nzchar(ap_prefix)) ap_prefix else "P",
      rfcs      = isolate(shared$interco_v2())$rfcs %||% character(),
      companies = companies
    )

    saved_ok <- tryCatch({
      save_interco_v2(registry)
      TRUE
    }, error = function(e) {
      showNotification(
        paste("\u274c Error al guardar en S3:", e$message),
        type = "error", duration = 10
      )
      FALSE
    })

    if (!saved_ok) return()

    shared$interco_v2(registry)

    # Re-render code lists from the clean saved state
    for (ini in names(cmap)) {
      co <- companies[[ini]]
      ic_slots[[paste0(ini, "_ar")]] <- max(1L, length(co$ar) + 1L)
      ic_slots[[paste0(ini, "_ap")]] <- max(1L, length(co$ap) + 1L)
    }
    ic_trigger(ic_trigger() + 1L)

    msg <- if (had_prefix)
      "\u2705 Guardado. Se eliminaron prefijos de letra autom\u00e1ticamente."
    else
      "\u2705 Configuraci\u00f3n intercompany guardada en S3."
    showNotification(msg, type = "message", duration = 4)
  }, ignoreInit = TRUE)

  # ── IC Scanner (dev only) ─────────────────────────────────────────────────────
  # Scans loaded SAP snapshots for unique CardCodes + RFCs, presents them in a
  # selectable DT so the dev can identify IC codes in one pass during setup.

  ic_scan_candidates <- reactiveVal(NULL)
  ic_code_lookup     <- reactiveVal(list())   # ledger → ini → code → nombre

  # Build code→name lookup from S3 snapshots.
  # Used to label IC code inputs with the matched company name.
  .build_code_lookup <- function() {
    snap_ar <- tryCatch(load_sap_snapshot("AR")$data, error = function(e) NULL)
    snap_ap <- tryCatch(load_sap_snapshot("AP")$data, error = function(e) NULL)
    cmap    <- shared$company_map()
    inv_map <- setNames(names(cmap), unname(cmap))

    extract_lkp <- function(df, code_col) {
      if (is.null(df) || !is.data.frame(df) || !code_col %in% names(df)) return(tibble())
      df |>
        dplyr::filter(!is.na(.data[[code_col]]), nzchar(trimws(.data[[code_col]])),
                      !is.na(Parte), nzchar(Parte)) |>
        dplyr::transmute(
          ini    = unname(inv_map[Empresa]),
          code   = toupper(trimws(.data[[code_col]])),
          nombre = Parte
        ) |>
        dplyr::filter(!is.na(ini)) |>
        dplyr::group_by(ini, code) |>
        dplyr::summarise(nombre = dplyr::first(nombre), .groups = "drop")
    }

    to_named_list <- function(df) {
      if (!nrow(df)) return(list())
      setNames(lapply(unique(df$ini), function(i) {
        rows <- df[df$ini == i, ]
        as.list(setNames(rows$nombre, rows$code))
      }), unique(df$ini))
    }

    list(
      ar = to_named_list(extract_lkp(snap_ar, "C\u00f3digo de cliente")),
      ap = to_named_list(extract_lkp(snap_ap, "C\u00f3digo de proveedor"))
    )
  }

  observeEvent(input$scan_ic_btn, {
    # Prefer S3 snapshots — they include ALL companies even when SAP was unreachable
    # this session.  Fall back to in-memory session data when snapshots are absent.
    sap_ar <- tryCatch(load_sap_snapshot("AR")$data, error = function(e) NULL) %||%
              shared$sap_data()[["AR"]]
    sap_ap <- tryCatch(load_sap_snapshot("AP")$data, error = function(e) NULL) %||%
              shared$sap_data()[["AP"]]

    if (is.null(sap_ar) && is.null(sap_ap)) {
      showNotification("No hay datos SAP disponibles. Refresca los datos primero.",
                       type = "warning")
      return()
    }

    candidates <- scan_ic_candidates(sap_ar, sap_ap, shared$interco_v2(), shared$company_map())

    if (!nrow(candidates)) {
      showNotification("No se encontraron c\u00f3digos en las facturas cargadas.",
                       type = "warning")
      return()
    }

    # Sort candidates to EXACTLY match DT display order so rows_selected indices align.
    # DT col 0 = Empresa string (asc), col 1 = Ledger "CxC"/"CxP" (asc), col 5 = Facturas (desc).
    # IMPORTANT: sort by the DISPLAY ledger label ("CxC"/"CxP"), NOT internal "AR"/"AP",
    # because "AP"<"AR" but "CxC"<"CxP" — opposite alphabetical order, which causes index misalignment.
    cmap          <- shared$company_map()
    emp_labels    <- paste0(candidates$initials, " \u2014 ",
                            unname(cmap[candidates$initials]))
    ledger_labels <- dplyr::if_else(candidates$ledger == "AR", "CxC", "CxP")
    ord <- order(emp_labels, ledger_labels, -candidates$n_facturas)
    candidates <- candidates[ord, ]
    ic_scan_candidates(candidates)

    presel <- which(candidates$is_ic)

    tbl <- candidates |>
      dplyr::transmute(
        Empresa     = paste0(initials, " \u2014 ",
                             unname(cmap[initials]) %||% initials),
        Ledger      = dplyr::if_else(ledger == "AR", "CxC", "CxP"),
        Codigo      = code,
        NombreSAP   = nombre,
        RFC         = dplyr::if_else(is.na(rfc) | !nzchar(rfc %||% ""), "\u2014", rfc),
        Facturas    = n_facturas
      )
    names(tbl)[3] <- "C\u00f3digo"
    names(tbl)[4] <- "Nombre SAP"

    showModal(modalDialog(
      title     = tagList(icon("magnifying-glass"),
                          " Esc\u00e1ner IC \u2014 facturas en memoria"),
      size      = "xl",
      easyClose = FALSE,
      p(class = "text-muted small mb-2",
        "Selecciona los c\u00f3digos que son Intercompany.",
        "Los ya configurados aparecen pre-seleccionados.",
        "Aplicar", strong("agrega"), "los c\u00f3digos seleccionados \u2014 no elimina los existentes.",
        "Para eliminar un c\u00f3digo, borra su campo en la pesta\u00f1a Intercompany."),
      DT::DTOutput("ic_scan_tbl"),
      footer = tagList(
        actionButton("ic_scan_apply",
                     tagList(icon("check"), " Aplicar selecci\u00f3n"),
                     class = "btn btn-primary btn-sm"),
        tags$span("\u00a0"),
        modalButton("Cancelar")
      )
    ))

    output$ic_scan_tbl <- DT::renderDT({
      DT::datatable(
        tbl,
        selection  = list(mode = "multiple", selected = presel),
        rownames   = FALSE,
        class      = "table table-sm table-hover",
        options    = list(
          pageLength = 30,
          order      = list(list(0L, "asc"), list(1L, "asc"), list(5L, "desc"))
        )
      )
    }, server = FALSE)
  })

  observeEvent(input$ic_scan_apply, {
    sel        <- input$ic_scan_tbl_rows_selected
    candidates <- ic_scan_candidates()
    removeModal()

    if (is.null(candidates) || !length(sel)) {
      showNotification("No se seleccionaron c\u00f3digos.", type = "warning")
      return()
    }

    selected  <- candidates[sel, ]
    cur       <- shared$interco_v2()
    ar_prefix <- toupper(cur$ar_prefix %||% "C")
    ap_prefix <- toupper(cur$ap_prefix %||% "P")

    # Strip whichever prefix matches (AR or AP) to get the raw numeric part.
    # SAP B1 uses the same CardCode in both AR and AP invoices for the same BP,
    # so the same numeric suffix appears in both snapshots — just with different
    # leading letters (C1027 in AR, P1027 in AP, or sometimes C1027 in both).
    # Stripping both prefixes and mirroring to both directions ensures the
    # filter works regardless of which snapshot the code was scanned from.
    strip_to_numeric <- function(code) {
      code <- toupper(trimws(code))
      s <- sub(paste0("^", ar_prefix), "", code)
      if (s != code) return(s)
      s <- sub(paste0("^", ap_prefix), "", code)
      if (s != code) return(s)
      code   # no known prefix — store as-is
    }

    cmap <- isolate(shared$company_map())

    new_companies <- setNames(
      lapply(names(cmap), function(ini) {
        ini_rows <- selected[selected$initials == ini, ]
        if (!nrow(ini_rows)) {
          # No scanner hits for this company — preserve any existing registry codes
          # rather than wiping them (company may just have no open IC invoices right now)
          existing <- cur$companies[[ini]] %||% list(ar = character(), ap = character())
          return(existing)
        }
        existing <- cur$companies[[ini]] %||% list(ar = character(), ap = character())
        ar_rows  <- ini_rows[ini_rows$ledger == "AR", ]
        ap_rows  <- ini_rows[ini_rows$ledger == "AP", ]
        # MERGE: union selected codes with existing so that pagination never
        # silently drops codes the user didn't scroll past this session.
        # To remove a code, delete it manually from the IC settings inputs.
        new_ar <- if (nrow(ar_rows)) { v <- unique(sapply(ar_rows$code, strip_to_numeric)); v[nzchar(v)] } else character()
        new_ap <- if (nrow(ap_rows)) { v <- unique(sapply(ap_rows$code, strip_to_numeric)); v[nzchar(v)] } else character()
        list(
          ar = unique(c(existing$ar %||% character(), new_ar)),
          ap = unique(c(existing$ap %||% character(), new_ap))
        )
      }),
      names(cmap)
    )

    # RFC lookup: full_code → RFC (named character vector, merged with existing)
    rfc_rows  <- selected[!is.na(selected$rfc) & nzchar(selected$rfc %||% ""), ]
    new_rfcs  <- cur$rfcs %||% character()
    if (nrow(rfc_rows)) {
      extra <- setNames(rfc_rows$rfc, rfc_rows$code)
      new_rfcs[names(extra)] <- extra
    }

    new_registry <- list(
      ar_prefix = cur$ar_prefix %||% "C",
      ap_prefix = cur$ap_prefix %||% "P",
      rfcs      = new_rfcs,
      companies = new_companies
    )

    # Log what we're about to save so mis-matches are visible in console
    for (ini in names(cmap)) {
      co <- new_companies[[ini]]
      message("[IC_SCAN_APPLY] ", ini,
              "  AR=", paste(co$ar, collapse=",") %||% "(none)",
              "  AP=", paste(co$ap, collapse=",") %||% "(none)")
    }

    shared$interco_v2(new_registry)
    # Auto-persist scanner results immediately — no manual Guardar required
    tryCatch(
      save_interco_v2(new_registry),
      error = function(e)
        showNotification(paste("No se pudo guardar en S3:", e$message), type = "warning")
    )

    # The scanner modal replaced the settings modal (Shiny supports only one modal at a time).
    # Re-open the settings modal shell, then immediately navigate to the IC panel so
    # the user sees the freshly loaded codes without having to reopen settings manually.
    show_settings_modal(input, output, session, shared)
    .show_ic_panel()

    showNotification(
      sprintf("%d c\u00f3digo(s) IC guardados.", nrow(selected)),
      type = "message", duration = 4
    )
  })

  # ── Business Partners ─────────────────────────────────────────────────────────
  observeEvent(input$stg_btn_catalogo, {
    if (.ic_has_changes()) {
      ic_pending_nav("catalogo")
      showModal(.ic_unsaved_modal())
      return()
    }
    ic_panel_active(FALSE)
    # Populate bp_candidates on first open (or if empty)
    if (is.null(bp_candidates())) {
      snap_ar <- tryCatch(load_sap_snapshot("AR"), error = function(e) NULL)
      snap_ap <- tryCatch(load_sap_snapshot("AP"), error = function(e) NULL)
      sap_ar  <- snap_ar$data
      sap_ap  <- snap_ap$data
      if (!is.null(sap_ar) || !is.null(sap_ap)) {
        cands <- scan_ic_candidates(sap_ar, sap_ap, isolate(shared$interco_v2()), isolate(shared$company_map()))
        bp_candidates(cands)
      }
    }
    output$settings_panel <- renderUI({
      .bp_panel_ui(shared, bp_candidates())
    })
  }, ignoreInit = TRUE)

  # Refresh SAP BP table
  observeEvent(input$bp_refresh_btn, {
    snap_ar <- tryCatch(load_sap_snapshot("AR"), error = function(e) NULL)
    snap_ap <- tryCatch(load_sap_snapshot("AP"), error = function(e) NULL)
    sap_ar  <- snap_ar$data
    sap_ap  <- snap_ap$data
    if (is.null(sap_ar) && is.null(sap_ap)) {
      showNotification("No hay snapshots SAP disponibles.", type = "warning")
      return()
    }
    cands <- scan_ic_candidates(sap_ar, sap_ap, isolate(shared$interco_v2()))
    bp_candidates(cands)
    output$settings_panel <- renderUI({ .bp_panel_ui(shared, bp_candidates()) })
    showNotification("Tabla de Business Partners actualizada.", type = "message", duration = 3)
  })

  # Apply selected BP rows as IC codes
  observeEvent(input$bp_apply_ic_btn, {
    sel      <- input$tbl_bp_sap_rows_selected
    sorted_c <- bp_sorted_candidates()
    if (is.null(sorted_c) || !length(sel)) {
      showNotification("Selecciona al menos un c\u00f3digo primero.", type = "warning")
      return()
    }

    selected  <- sorted_c[sel, ]
    cur       <- shared$interco_v2()
    ar_prefix <- toupper(cur$ar_prefix %||% "C")
    ap_prefix <- toupper(cur$ap_prefix %||% "P")

    strip_num <- function(code) {
      code <- toupper(trimws(code))
      s <- sub(paste0("^", ar_prefix), "", code); if (s != code) return(s)
      s <- sub(paste0("^", ap_prefix), "", code); if (s != code) return(s)
      code
    }

    cmap <- isolate(shared$company_map())

    new_companies <- setNames(
      lapply(names(cmap), function(ini) {
        ini_rows <- selected[selected$initials == ini, ]
        existing <- cur$companies[[ini]] %||% list(ar = character(), ap = character())
        if (!nrow(ini_rows)) return(existing)
        ar_rows <- ini_rows[ini_rows$ledger == "AR", ]
        ap_rows <- ini_rows[ini_rows$ledger == "AP", ]
        new_ar  <- if (nrow(ar_rows)) { v <- unique(sapply(ar_rows$code, strip_num)); v[nzchar(v)] } else character()
        new_ap  <- if (nrow(ap_rows)) { v <- unique(sapply(ap_rows$code, strip_num)); v[nzchar(v)] } else character()
        list(
          ar = unique(c(existing$ar %||% character(), new_ar)),
          ap = unique(c(existing$ap %||% character(), new_ap))
        )
      }),
      names(cmap)
    )

    rfc_rows <- selected[!is.na(selected$rfc) & nzchar(selected$rfc %||% ""), ]
    new_rfcs <- cur$rfcs %||% character()
    if (nrow(rfc_rows)) new_rfcs[rfc_rows$code] <- rfc_rows$rfc

    new_registry <- list(
      ar_prefix = cur$ar_prefix %||% "C",
      ap_prefix = cur$ap_prefix %||% "P",
      rfcs      = new_rfcs,
      companies = new_companies
    )

    shared$interco_v2(new_registry)
    tryCatch(
      save_interco_v2(new_registry),
      error = function(e)
        showNotification(paste("No se pudo guardar en S3:", e$message), type = "warning")
    )

    # Refresh is_ic flags in bp_candidates so preselection updates
    new_cands <- tryCatch({
      sap_ar <- tryCatch(load_sap_snapshot("AR")$data, error = function(e) NULL)
      sap_ap <- tryCatch(load_sap_snapshot("AP")$data, error = function(e) NULL)
      scan_ic_candidates(sap_ar, sap_ap, new_registry, isolate(shared$company_map()))
    }, error = function(e) NULL)
    if (!is.null(new_cands) && nrow(new_cands)) bp_candidates(new_cands)

    showNotification(
      sprintf("%d c\u00f3digo(s) IC guardados desde Business Partners.", nrow(selected)),
      type = "message", duration = 4
    )
  })

  # Render SAP BP table with auto-match against catálogo.
  # Sort to match DT display order so row indices align with bp_sorted_candidates().
  output$tbl_bp_sap <- DT::renderDT({
    cands <- bp_candidates()
    req(!is.null(cands), nrow(cands) > 0)
    cmap    <- shared$company_map()
    prov    <- tryCatch(shared$proveedores_db(), error = function(e) NULL)
    emp_lbl <- paste0(cands$initials, " \u2014 ",
                      unname(cmap[cands$initials]) %||% cands$initials)
    ldg_lbl <- dplyr::if_else(cands$ledger == "AR", "CxC", "CxP")
    ord     <- order(emp_lbl, ldg_lbl, -cands$n_facturas)
    sorted  <- cands[ord, ]
    bp_sorted_candidates(sorted)
    presel  <- which(sorted$is_ic)
    .bp_datatable(sorted, prov, presel = presel, cmap = cmap)
  }, server = FALSE)

  output$tbl_catalogo <- DT::renderDataTable({
    .catalogo_datatable(shared)
  }, server = TRUE)

  observeEvent(input$cat_empresa_filter, {
    output$tbl_catalogo <- DT::renderDataTable({
      .catalogo_datatable(shared, empresa_filter = input$cat_empresa_filter)
    }, server = TRUE)
  }, ignoreInit = TRUE)

  # ── Cuentas de Empresa ────────────────────────────────────────────────────────
  observeEvent(input$stg_btn_cuentas, {
    if (.ic_has_changes()) {
      ic_pending_nav("cuentas")
      showModal(.ic_unsaved_modal())
      return()
    }
    ic_panel_active(FALSE)
    output$settings_panel <- renderUI({ .cuentas_panel_ui(cmap = shared$company_map()) })
  }, ignoreInit = TRUE)

  proveedoresServer("prov_mod", shared)

}

# =============================================================================
# Business Partners
# =============================================================================
.bp_panel_ui <- function(shared, candidates) {
  div(
    # ── Section 1: SAP Business Partners ────────────────────────────────────
    div(class = "d-flex align-items-center gap-2 mb-2",
      tags$h6(class = "fw-semibold mb-0",
              tagList(icon("building"), " Business Partners (SAP)")),
      tags$span(class = "text-muted small",
        "— C\u00f3digos \u00fanicos extra\u00eddos de los snapshots de facturas"),
      div(class = "ms-auto",
        actionButton("bp_refresh_btn", tagList(icon("rotate"), " Actualizar"),
                     class = "btn btn-sm btn-outline-secondary")
      )
    ),
    if (is.null(candidates) || !nrow(candidates)) {
      div(class = "alert alert-secondary small py-2 mb-3",
          icon("circle-info"), " No hay datos SAP cargados. ",
          "Haz clic en \u201cActualizar\u201d o carga facturas primero.")
    } else {
      tagList(
        tags$p(class = "text-muted small mb-1",
               "Selecciona los c\u00f3digos IC y haz clic en",
               strong("Aplicar como IC"), ". Pre-seleccionados = ya configurados.",
               "Aplicar", strong("agrega"), "c\u00f3digos; para eliminar uno, borra su campo en Intercompany."),
        div(style = "max-height: 260px; overflow-y: auto; margin-bottom: 0.5rem;",
            DT::DTOutput("tbl_bp_sap")),
        actionButton("bp_apply_ic_btn",
                     tagList(icon("arrows-left-right"), " Aplicar como IC"),
                     class = "btn btn-primary btn-sm mb-3")
      )
    },

    tags$hr(class = "my-3"),

    # ── Section 2: Catálogo de Proveedores ───────────────────────────────────
    div(class = "d-flex align-items-center gap-2 mb-3",
      tags$h6(class = "fw-semibold mb-0",
              tagList(icon("address-book"), " Cat\u00e1logo de Proveedores")),
      tags$span(class = "text-muted small",
        "— Alias Baj\u00edo + CLABE para generar archivos PPL")
    ),
    proveedoresUI("prov_mod")
  )
}

# Build the SAP BP datatable with auto-match column
.bp_datatable <- function(candidates, prov_db = NULL, presel = integer(0),
                           cmap = COMPANY_MAP) {
  tbl <- candidates |>
    dplyr::transmute(
      Empresa   = paste0(initials, " \u2014 ",
                         unname(cmap[initials]) %||% initials),
      Ledger    = dplyr::if_else(ledger == "AR", "CxC", "CxP"),
      Codigo    = code,
      Nombre    = dplyr::coalesce(nombre, "\u2014"),
      RFC       = dplyr::if_else(is.na(rfc) | !nzchar(rfc %||% ""), "\u2014", rfc),
      Facturas  = n_facturas,
      .rfc_raw  = rfc,
      .code_raw = code
    )

  # Auto-match: mark codes already in catálogo via RFC or CardCode
  if (!is.null(prov_db) && nrow(prov_db) > 0) {
    cat_rfcs   <- toupper(trimws(prov_db$rfc   %||% character()))
    cat_codes  <- toupper(trimws(prov_db$codigo %||% character()))
    tbl <- tbl |>
      dplyr::mutate(
        En_Catalogo = dplyr::case_when(
          toupper(trimws(.rfc_raw))  %in% cat_rfcs[nzchar(cat_rfcs)]   ~ "\u2705 RFC",
          toupper(trimws(.code_raw)) %in% cat_codes[nzchar(cat_codes)] ~ "\u2705 C\u00f3digo",
          TRUE ~ "\u2014"
        )
      )
  } else {
    tbl$En_Catalogo <- "\u2014"
  }

  tbl <- dplyr::select(tbl, -dplyr::starts_with("."))
  names(tbl)[names(tbl) == "En_Catalogo"] <- "En Cat\u00e1logo"

  DT::datatable(
    tbl,
    rownames  = FALSE,
    class     = "table table-sm table-hover",
    selection = list(mode = "multiple", selected = presel),
    options   = list(
      pageLength = 20,
      order      = list(list(0L, "asc"), list(1L, "asc"), list(5L, "desc")),
      dom        = "ftp"
    )
  )
}

# =============================================================================
# Catálogo de Proveedores
# =============================================================================
.catalogo_panel_ui <- function(cmap = COMPANY_MAP) {
  div(
    div(class = "d-flex align-items-center gap-2 mb-3",
      tags$h6(class = "fw-semibold mb-0",
              tagList(icon("address-book"), " Catálogo de Proveedores")),
      tags$span(class = "text-muted small",
        "— Mapea el nombre SAP \u2192 alias Baj\u00edo + CLABE para generar archivos PPL")
    ),
    div(class = "d-flex align-items-center gap-2 mb-2",
      div(style = "width: 180px;",
        selectInput("cat_empresa_filter", NULL,
                    choices = c("Todas" = "", setNames(names(cmap), unname(cmap))),
                    selected = "", width = "100%")
      ),
      div(class = "ms-auto",
        actionButton("stg_new_proveedor", tagList(icon("plus"), " Nuevo proveedor"),
                     class = "btn btn-sm btn-outline-primary")
      )
    ),
    div(class = "d-flex gap-3 align-items-center mb-2",
      tags$span(class = "small text-muted", "Estado:"),
      tags$span(class = "ph-legend-dot ph-ok"),
      tags$span(class = "small", "Completo"),
      tags$span(class = "ph-legend-dot ph-partial"),
      tags$span(class = "small text-warning", "Sin CLABE"),
      tags$span(class = "ph-legend-dot ph-nomatch"),
      tags$span(class = "small text-danger", "Sin alias")
    ),
    div(style = "max-height: 300px; overflow-y: auto;",
      DT::dataTableOutput("tbl_catalogo")
    ),
    tags$hr(),
    uiOutput("catalogo_edit_form")
  )
}

.catalogo_datatable <- function(shared, empresa_filter = "") {
  provs <- tryCatch(
    if (!is.null(shared$proveedores_db)) shared$proveedores_db()
    else load_proveedores(),
    error = function(e) tibble::tibble()
  )
  if (!nrow(provs)) {
    return(DT::datatable(
      data.frame(Info = "Sin proveedores. Haz clic en '+ Nuevo proveedor'."),
      options = list(dom = "t"), rownames = FALSE
    ))
  }
  if (nzchar(empresa_filter))
    provs <- provs |> dplyr::filter(Empresa == empresa_filter | Empresa == "")

  disp <- provs |>
    dplyr::mutate(
      Estado = dplyr::case_when(
        nzchar(trimws(alias %||% "")) & nzchar(trimws(clabe %||% "")) &
          clabe != "000000000000000000" ~ "\u2705",
        nzchar(trimws(alias %||% "")) ~ "\u26a0\ufe0f",
        TRUE ~ "\u274c"
      ),
      Alias_disp = dplyr::if_else(
        !is.na(alias) & nzchar(alias),
        paste0(alias, dplyr::if_else(nchar(alias) > 12,
          paste0(" <span class='text-warning small'>(", nchar(alias), "/15)</span>"), "")),
        "<span class='text-muted fst-italic small'>sin alias</span>"
      ),
      CLABE_disp = dplyr::if_else(
        !is.na(clabe) & nzchar(clabe) & clabe != "000000000000000000",
        paste0('<span class="font-monospace small">', clabe, '</span>'),
        "<span class='text-muted fst-italic small'>sin CLABE</span>"
      )
    ) |>
    dplyr::select(Estado, Empresa, `Nombre SAP` = nombre, Alias = Alias_disp,
                  CLABE = CLABE_disp, Medio = medio_pago, RFC = rfc)

  DT::datatable(disp, escape = FALSE, rownames = FALSE, selection = "single",
    options = list(pageLength = 15, dom = "ftp", scrollX = TRUE,
      columnDefs = list(
        list(width = "28px",  targets = 0), list(width = "55px",  targets = 1),
        list(width = "200px", targets = 2), list(width = "110px", targets = 3),
        list(width = "160px", targets = 4), list(width = "50px",  targets = 5),
        list(width = "100px", targets = 6)
      )
    )
  )
}

settings_catalogo_edit_observer <- function(input, output, session, shared) {
  editing_id <- reactiveVal(NULL)

  observeEvent(input$tbl_catalogo_rows_selected, {
    sel <- input$tbl_catalogo_rows_selected
    if (!length(sel)) {
      editing_id(NULL)
      output$catalogo_edit_form <- renderUI({ .catalogo_form_empty() }); return()
    }
    provs <- tryCatch(
      if (!is.null(shared$proveedores_db)) shared$proveedores_db() else load_proveedores(),
      error = function(e) tibble::tibble()
    )
    ef <- isolate(input$cat_empresa_filter %||% "")
    if (nzchar(ef)) provs <- provs |> dplyr::filter(Empresa == ef | Empresa == "")
    if (!nrow(provs) || sel > nrow(provs)) return()
    row <- provs[sel, ]
    editing_id(row$id)
    output$catalogo_edit_form <- renderUI({ .catalogo_form_ui(row, cmap = shared$company_map()) })
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  observeEvent(input$stg_new_proveedor, {
    editing_id(NULL)
    output$catalogo_edit_form <- renderUI({ .catalogo_form_ui(NULL, cmap = shared$company_map()) })
  }, ignoreInit = TRUE)

  observeEvent(input$cat_save_row, {
    eid       <- editing_id()
    provs     <- tryCatch(load_proveedores(), error = function(e) .schema_proveedores_local())
    alias_val <- trimws(input$cat_alias %||% "")
    if (nchar(alias_val) > 15) {
      showNotification("El alias no puede tener m\u00e1s de 15 caracteres.", type = "warning")
      return()
    }
    clabe_val <- trimws(input$cat_clabe %||% "")
    medio_val <- input$cat_medio %||% "SPI"
    if (medio_val == "SPI" && nzchar(clabe_val) && nchar(clabe_val) != 18) {
      showNotification("La CLABE debe tener exactamente 18 d\u00edgitos.", type = "warning")
      return()
    }
    new_row <- tibble::tibble(
      id = if (is.null(eid)) uuid::UUIDgenerate() else eid,
      Empresa = input$cat_empresa %||% "", codigo = trimws(input$cat_codigo %||% ""),
      nombre = trimws(input$cat_nombre %||% ""), alias = alias_val,
      clabe = clabe_val, medio_pago = medio_val, rfc = trimws(input$cat_rfc %||% ""),
      tipo = if (medio_val == "BCO") "021" else "012",
      banco_destino = NA_character_, activo = TRUE
    )
    if (is.null(eid)) {
      dup <- provs |> dplyr::filter(
        Empresa == new_row$Empresa, toupper(alias) == toupper(alias_val), activo == TRUE)
      if (nrow(dup)) {
        showNotification(paste0("El alias '", alias_val, "' ya existe."), type = "warning")
        return()
      }
      updated <- dplyr::bind_rows(provs, new_row)
    } else {
      updated <- provs |> dplyr::rows_update(new_row, by = "id", unmatched = "ignore")
    }
    save_proveedores(updated)
    if (!is.null(shared$proveedores_db)) shared$proveedores_db(updated)
    editing_id(NULL)
    output$catalogo_edit_form <- renderUI({ .catalogo_form_empty() })
    output$tbl_catalogo <- DT::renderDataTable({
      .catalogo_datatable(shared, input$cat_empresa_filter %||% "")
    }, server = TRUE)
    showNotification(
      if (is.null(eid)) "Proveedor agregado." else "Proveedor actualizado.",
      type = "message", duration = 2)
  }, ignoreInit = TRUE)

  observeEvent(input$cat_delete_row, {
    eid <- editing_id(); if (is.null(eid)) return()
    provs   <- tryCatch(load_proveedores(), error = function(e) .schema_proveedores_local())
    updated <- provs |> dplyr::mutate(activo = dplyr::if_else(id == eid, FALSE, activo))
    save_proveedores(updated)
    if (!is.null(shared$proveedores_db)) shared$proveedores_db(updated)
    editing_id(NULL)
    output$catalogo_edit_form <- renderUI({ .catalogo_form_empty() })
    output$tbl_catalogo <- DT::renderDataTable({
      .catalogo_datatable(shared, input$cat_empresa_filter %||% "")
    }, server = TRUE)
    showNotification("Proveedor eliminado.", type = "message", duration = 2)
  }, ignoreInit = TRUE)
}

.catalogo_form_empty <- function() {
  div(class = "text-muted small fst-italic pt-1",
    icon("hand-pointer"),
    " Selecciona un proveedor para editarlo, o haz clic en '+ Nuevo proveedor'.")
}

.catalogo_form_ui <- function(row = NULL, cmap = COMPANY_MAP) {
  is_new  <- is.null(row)
  emp_val <- if (!is_new) row$Empresa    %||% "" else ""
  nom_val <- if (!is_new) row$nombre     %||% "" else ""
  ali_val <- if (!is_new) row$alias      %||% "" else ""
  cla_val <- if (!is_new) row$clabe      %||% "" else ""
  med_val <- if (!is_new) row$medio_pago %||% "SPI" else "SPI"
  rfc_val <- if (!is_new) row$rfc        %||% "" else ""
  cod_val <- if (!is_new) row$codigo     %||% "" else ""

  div(class = "catalogo-form border rounded p-3 bg-light mt-2",
    tags$h6(class = "fw-semibold mb-3",
      if (is_new) tagList(icon("plus"), " Nuevo proveedor")
      else        tagList(icon("pencil"), " Editar proveedor")),
    div(class = "row g-2",
      div(class = "col-md-3",
        tags$label("Empresa", class = "form-label small fw-semibold mb-1"),
        selectInput("cat_empresa", NULL,
          choices = c("Todas" = "", setNames(names(cmap), unname(cmap))),
          selected = emp_val, width = "100%")
      ),
      div(class = "col-md-9",
        tags$label(tagList("Nombre SAP",
          tags$span(class = "text-muted fw-normal", " \u2014 exactamente como aparece en CxP")),
          class = "form-label small fw-semibold mb-1"),
        textInput("cat_nombre", NULL, value = nom_val, width = "100%",
                  placeholder = "TRANSPORTES FICTICIOS SA DE CV")
      ),
      div(class = "col-md-4",
        tags$label(tagList("Alias Baj\u00edo",
          tags$span(class = "badge bg-primary ms-1", "m\u00e1x 15")),
          class = "form-label small fw-semibold mb-1"),
        textInput("cat_alias", NULL, value = ali_val, width = "100%",
                  placeholder = "TRANSFICTICIAS"),
        uiOutput("cat_alias_counter")
      ),
      div(class = "col-md-3",
        tags$label("Medio de pago", class = "form-label small fw-semibold mb-1"),
        selectInput("cat_medio", NULL, choices = c("SPI","BCO","TEF","SPD"),
                    selected = med_val, width = "100%")
      ),
      div(class = "col-md-5",
        tags$label(tagList("CLABE / Cuenta destino",
          tags$span(class = "text-muted fw-normal", " \u2014 18 d\u00edgitos")),
          class = "form-label small fw-semibold mb-1"),
        textInput("cat_clabe", NULL, value = cla_val, width = "100%",
                  placeholder = "012310098765432101")
      ),
      div(class = "col-md-4",
        tags$label("RFC", class = "form-label small fw-semibold mb-1"),
        textInput("cat_rfc", NULL, value = rfc_val, width = "100%",
                  placeholder = "TFS850312HX1")
      ),
      div(class = "col-md-4",
        tags$label("C\u00f3digo SAP", class = "form-label small fw-semibold mb-1"),
        textInput("cat_codigo", NULL, value = cod_val, width = "100%",
                  placeholder = "V00456")
      )
    ),
    div(class = "d-flex gap-2 mt-3",
      actionButton("cat_save_row",
        tagList(icon("floppy-disk"), if (is_new) " Agregar" else " Guardar cambios"),
        class = "btn btn-primary btn-sm"),
      if (!is_new)
        actionButton("cat_delete_row", tagList(icon("trash"), " Eliminar"),
                     class = "btn btn-outline-danger btn-sm") else NULL,
      actionButton("cat_cancel_edit", "Cancelar",
                   class = "btn btn-outline-secondary btn-sm")
    )
  )
}

settings_alias_counter <- function(input, output, session) {
  output$cat_alias_counter <- renderUI({
    val <- trimws(input$cat_alias %||% "")
    n   <- nchar(val)
    if (n == 0) return(NULL)
    cls <- if (n > 15) "text-danger fw-bold" else if (n > 12) "text-warning" else "text-success"
    tags$span(class = paste("small", cls), paste0(n, " / 15 caracteres"))
  })
}

settings_cancel_edit_observer <- function(input, output, session) {
  observeEvent(input$cat_cancel_edit, {
    output$catalogo_edit_form <- renderUI({ .catalogo_form_empty() })
  }, ignoreInit = TRUE)
}

.schema_proveedores_local <- function() tibble::tibble(
  id = character(), Empresa = character(), codigo = character(),
  nombre = character(), alias = character(), clabe = character(),
  medio_pago = character(), rfc = character(), tipo = character(),
  banco_destino = character(), activo = logical()
)

# =============================================================================
# Cuentas de Empresa
# =============================================================================
.cuentas_panel_ui <- function(cmap = COMPANY_MAP) {
  div(
    div(class = "d-flex align-items-center gap-2 mb-3",
      tags$h6(class = "fw-semibold mb-0",
        tagList(icon("building-columns"), " Cuentas de Empresa")),
      tags$span(class = "text-muted small",
        "— La cuenta marcada ",
        tags$span(class = "badge bg-primary", "PPL"),
        " se usa como cuenta origen al generar archivos Baj\u00edo.")
    ),

    div(class = "row g-3",

      # ── Banco catalog (left) ─────────────────────────────────────────────
      div(class = "col-md-4",
        div(class = "card h-100",
          div(class = "card-header d-flex align-items-center py-2",
            tags$span(class = "fw-semibold small flex-grow-1",
                      tagList(icon("university"), " Bancos")),
            actionButton("cta_new_banco", tagList(icon("plus"), " Banco"),
                         class = "btn btn-sm btn-outline-primary py-0 px-2")
          ),
          div(class = "card-body p-0", style = "max-height: 260px; overflow-y: auto;",
            DT::dataTableOutput("tbl_bancos_cat")
          )
        )
      ),

      # ── Accounts (right) ─────────────────────────────────────────────────
      div(class = "col-md-8",
        div(class = "card h-100",
          div(class = "card-header d-flex align-items-center gap-2 py-2",
            tags$span(class = "fw-semibold small", tagList(icon("credit-card"), " Cuentas")),
            div(class = "ms-auto d-flex gap-2",
              div(style = "width:175px;",
                selectInput("cta_empresa_filter", NULL,
                  choices = c("Todas las empresas" = "",
                              setNames(names(cmap), unname(cmap))),
                  width = "100%")
              ),
              actionButton("cta_new_cuenta", tagList(icon("plus"), " Cuenta"),
                           class = "btn btn-sm btn-outline-primary")
            )
          ),
          div(class = "card-body p-0", style = "max-height: 260px; overflow-y: auto;",
            DT::dataTableOutput("tbl_cuentas")
          )
        )
      )
    ),

    tags$hr(class = "my-3"),
    uiOutput("cuentas_edit_form")
  )
}

.bancos_cat_datatable <- function() {
  bancos <- tryCatch(load_ctas_bancos(), error = function(e) .schema_ctas_bancos())
  if (!nrow(bancos)) {
    return(DT::datatable(
      data.frame(Info = "Sin bancos. Haz clic en '+ Banco'."),
      options = list(dom = "t"), rownames = FALSE
    ))
  }
  DT::datatable(
    bancos |> dplyr::select(Banco = nombre, Clave = clave),
    escape = FALSE, rownames = FALSE, selection = "single",
    options = list(dom = "t", pageLength = 30,
      columnDefs = list(list(width = "55px", targets = 1)))
  )
}

.cuentas_datatable <- function(shared, empresa_filter = "") {
  ctas <- tryCatch({
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas()
    else load_ctas_cuentas()
  }, error = function(e) .schema_ctas_cuentas())
  bancos <- tryCatch(load_ctas_bancos(), error = function(e) .schema_ctas_bancos())

  if (!nrow(ctas)) {
    return(DT::datatable(
      data.frame(Info = "Sin cuentas. Haz clic en '+ Cuenta'."),
      options = list(dom = "t"), rownames = FALSE
    ))
  }
  if (nzchar(empresa_filter)) ctas <- ctas |> dplyr::filter(Empresa == empresa_filter)
  ctas <- ctas |> dplyr::filter(activa == TRUE)
  if (!nrow(ctas)) {
    return(DT::datatable(
      data.frame(Info = "Sin cuentas activas para esa empresa."),
      options = list(dom = "t"), rownames = FALSE
    ))
  }

  if (nrow(bancos)) {
    ctas <- ctas |>
      dplyr::left_join(bancos |> dplyr::select(banco_id = id, banco_nombre = nombre),
                       by = "banco_id")
  } else { ctas$banco_nombre <- NA_character_ }

  disp <- ctas |> dplyr::mutate(
    PPL = dplyr::if_else(isTRUE(is_ppl_default),
                         "<span class='badge bg-primary'>PPL</span>", ""),
    Cuenta_disp = paste0('<span class="font-monospace small">', cuenta, '</span>'),
    Alias_disp  = dplyr::if_else(
      !is.na(alias) & nzchar(trimws(alias %||% "")), alias,
      "<span class='text-muted fst-italic small'>\u2014</span>")
  ) |> dplyr::select(
    PPL, Empresa, Banco = banco_nombre, Moneda,
    Alias = Alias_disp, `Cuenta origen` = Cuenta_disp, `Razon Social` = razon_social
  )

  DT::datatable(
    disp, escape = FALSE, rownames = FALSE, selection = "single",
    options = list(dom = "ftp", pageLength = 10, scrollX = TRUE,
      columnDefs = list(
        list(width = "40px",  targets = 0), list(width = "50px",  targets = 1),
        list(width = "90px",  targets = 2), list(width = "50px",  targets = 3),
        list(width = "90px",  targets = 4), list(width = "140px", targets = 5)
      )
    )
  )
}

# ── Cuentas observer (called once from app.R server) ──────────────────────────
settings_cuentas_observer <- function(input, output, session, shared) {

  editing_cuenta_id <- reactiveVal(NULL)
  editing_banco_id  <- reactiveVal(NULL)

  output$tbl_bancos_cat <- DT::renderDataTable({
    .bancos_cat_datatable()
  }, server = TRUE)

  output$tbl_cuentas <- DT::renderDataTable({
    .cuentas_datatable(shared, isolate(input$cta_empresa_filter %||% ""))
  }, server = TRUE)

  observeEvent(input$cta_empresa_filter, {
    output$tbl_cuentas <- DT::renderDataTable({
      .cuentas_datatable(shared, input$cta_empresa_filter %||% "")
    }, server = TRUE)
  }, ignoreInit = TRUE)

  # ── + Banco ──────────────────────────────────────────────────────────────────
  observeEvent(input$cta_new_banco, {
    editing_banco_id(NULL); editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .banco_form_ui(NULL) })
  }, ignoreInit = TRUE)

  # ── Select banco ─────────────────────────────────────────────────────────────
  observeEvent(input$tbl_bancos_cat_rows_selected, {
    sel <- input$tbl_bancos_cat_rows_selected
    if (!length(sel)) {
      editing_banco_id(NULL)
      output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() }); return()
    }
    bancos <- tryCatch(load_ctas_bancos(), error = function(e) .schema_ctas_bancos())
    if (!nrow(bancos) || sel > nrow(bancos)) return()
    row <- bancos[sel, ]
    editing_banco_id(row$id); editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .banco_form_ui(row) })
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # ── Save banco ────────────────────────────────────────────────────────────────
  observeEvent(input$banco_save, {
    eid    <- editing_banco_id()
    nombre <- trimws(input$banco_nombre %||% "")
    clave  <- trimws(input$banco_clave  %||% "")
    if (!nzchar(nombre)) {
      showNotification("El nombre del banco es obligatorio.", type = "warning"); return()
    }
    bancos  <- tryCatch(load_ctas_bancos(), error = function(e) .schema_ctas_bancos())
    new_row <- tibble::tibble(
      id = if (is.null(eid)) uuid::UUIDgenerate() else eid,
      nombre = nombre, clave = clave
    )
    updated <- if (is.null(eid)) dplyr::bind_rows(bancos, new_row)
               else bancos |> dplyr::rows_update(new_row, by = "id", unmatched = "ignore")
    save_ctas_bancos(updated)
    editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    output$tbl_bancos_cat <- DT::renderDataTable({ .bancos_cat_datatable() }, server = TRUE)
    showNotification(if (is.null(eid)) "Banco agregado." else "Banco actualizado.",
                     type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # ── Delete banco ──────────────────────────────────────────────────────────────
  observeEvent(input$banco_delete, {
    eid <- editing_banco_id(); if (is.null(eid)) return()
    bancos  <- tryCatch(load_ctas_bancos(), error = function(e) .schema_ctas_bancos())
    updated <- bancos |> dplyr::filter(id != eid)
    save_ctas_bancos(updated)
    editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    output$tbl_bancos_cat <- DT::renderDataTable({ .bancos_cat_datatable() }, server = TRUE)
    showNotification("Banco eliminado.", type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # ── + Cuenta ──────────────────────────────────────────────────────────────────
  observeEvent(input$cta_new_cuenta, {
    editing_cuenta_id(NULL); editing_banco_id(NULL)
    sel_b  <- input$tbl_bancos_cat_rows_selected
    bancos <- tryCatch(load_ctas_bancos(), error = function(e) .schema_ctas_bancos())
    pre_b  <- if (length(sel_b) && nrow(bancos) >= sel_b) bancos$id[sel_b] else NULL
    pre_e  <- isolate(input$cta_empresa_filter %||% "")
    output$cuentas_edit_form <- renderUI({
      .cuenta_form_ui(NULL, bancos, pre_b, pre_e, cmap = shared$company_map())
    })
  }, ignoreInit = TRUE)

  # ── Select cuenta ─────────────────────────────────────────────────────────────
  observeEvent(input$tbl_cuentas_rows_selected, {
    sel <- input$tbl_cuentas_rows_selected
    if (!length(sel)) {
      editing_cuenta_id(NULL)
      output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() }); return()
    }
    ctas <- tryCatch({
      if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas() else load_ctas_cuentas()
    }, error = function(e) .schema_ctas_cuentas())
    ef <- isolate(input$cta_empresa_filter %||% "")
    if (nzchar(ef)) ctas <- ctas |> dplyr::filter(Empresa == ef)
    ctas <- ctas |> dplyr::filter(activa == TRUE)
    if (!nrow(ctas) || sel > nrow(ctas)) return()
    row    <- ctas[sel, ]
    bancos <- tryCatch(load_ctas_bancos(), error = function(e) .schema_ctas_bancos())
    editing_cuenta_id(row$id); editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({
      .cuenta_form_ui(row, bancos, row$banco_id, ef, cmap = shared$company_map())
    })
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # ── Save cuenta ───────────────────────────────────────────────────────────────
  observeEvent(input$cuenta_save, {
    shinyjs::disable("cuenta_save")
    on.exit(shinyjs::enable("cuenta_save"), add = TRUE)

    eid         <- editing_cuenta_id()
    empresa_val <- input$cta_empresa %||% ""
    cuenta_val  <- trimws(input$cta_cuenta %||% "")
    moneda_val  <- input$cta_moneda  %||% "MXN"

    if (!nzchar(empresa_val)) {
      showNotification("Selecciona una empresa.", type = "warning"); return()
    }
    if (!nzchar(cuenta_val)) {
      showNotification("El n\u00famero de cuenta es obligatorio.", type = "warning"); return()
    }

    ctas       <- tryCatch({
      if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas() else load_ctas_cuentas()
    }, error = function(e) .schema_ctas_cuentas())
    is_default <- isTRUE(input$cta_is_ppl_default)

    # Clear PPL default flag from sibling accounts (same empresa + moneda)
    if (is_default) {
      if (!"is_ppl_default" %in% names(ctas)) ctas$is_ppl_default <- FALSE
      mask <- !is.na(ctas$Empresa) & ctas$Empresa == empresa_val &
              !is.na(ctas$Moneda)  & toupper(ctas$Moneda) == toupper(moneda_val) &
              (is.null(eid) | (!is.na(ctas$id) & ctas$id != eid))
      ctas$is_ppl_default[mask] <- FALSE
    }

    new_row <- tibble::tibble(
      id                  = if (is.null(eid)) uuid::UUIDgenerate() else eid,
      banco_id            = input$cta_banco_id %||% NA_character_,
      Empresa             = empresa_val,
      razon_social        = trimws(input$cta_razon_social %||% ""),
      rfc                 = trimws(input$cta_rfc          %||% ""),
      Moneda              = toupper(moneda_val),
      alias               = trimws(substr(input$cta_alias %||% "", 1, 15)),
      cuenta              = cuenta_val,
      clabe_interbancaria = trimws(input$cta_clabe        %||% ""),
      is_ppl_default      = is_default,
      saldo_inicial       = as.numeric(input$cta_saldo_inicial %||% 0),
      activa              = TRUE
    )

    updated <- if (is.null(eid)) dplyr::bind_rows(ctas, new_row)
               else dplyr::bind_rows(ctas |> dplyr::filter(id != eid), new_row)

    save_ctas_cuentas(updated)
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas(updated)

    editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    output$tbl_cuentas <- DT::renderDataTable({
      .cuentas_datatable(shared, isolate(input$cta_empresa_filter %||% ""))
    }, server = TRUE)
    showNotification(
      if (is.null(eid)) "Cuenta agregada." else "Cuenta actualizada.",
      type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # ── Delete cuenta ─────────────────────────────────────────────────────────────
  observeEvent(input$cuenta_delete, {
    eid <- editing_cuenta_id(); if (is.null(eid)) return()
    ctas <- tryCatch({
      if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas() else load_ctas_cuentas()
    }, error = function(e) .schema_ctas_cuentas())
    updated <- ctas |> dplyr::mutate(
      activa = dplyr::if_else(id == eid, FALSE, activa))
    save_ctas_cuentas(updated)
    if (!is.null(shared$ctas_cuentas)) shared$ctas_cuentas(updated)
    editing_cuenta_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
    output$tbl_cuentas <- DT::renderDataTable({
      .cuentas_datatable(shared, isolate(input$cta_empresa_filter %||% ""))
    }, server = TRUE)
    showNotification("Cuenta desactivada.", type = "message", duration = 2)
  }, ignoreInit = TRUE)

  # ── Cancel ────────────────────────────────────────────────────────────────────
  observeEvent(input$cta_cancel, {
    editing_cuenta_id(NULL); editing_banco_id(NULL)
    output$cuentas_edit_form <- renderUI({ .cuentas_form_empty() })
  }, ignoreInit = TRUE)
}

# ── Empty placeholder ─────────────────────────────────────────────────────────
.cuentas_form_empty <- function() {
  div(class = "text-muted small fst-italic",
    icon("hand-pointer"),
    " Selecciona un banco o cuenta para editar, o usa los botones + para agregar.")
}

# ── Bank form ─────────────────────────────────────────────────────────────────
.banco_form_ui <- function(row = NULL) {
  is_new  <- is.null(row)
  nom_val <- if (!is_new) row$nombre %||% "" else ""
  cla_val <- if (!is_new) row$clave  %||% "" else ""

  div(class = "catalogo-form border rounded p-3 bg-light",
    tags$h6(class = "fw-semibold mb-3",
      if (is_new) tagList(icon("plus"), " Agregar banco")
      else        tagList(icon("pencil"), " Editar banco")),

    div(class = "row g-2",
      div(class = "col-12 mb-1",
        tags$label("Selecci\u00f3n r\u00e1pida", class = "form-label small text-muted mb-1"),
        selectInput("banco_picker", NULL,
                    choices = .BANCOS_MX_CHOICES, selected = "", width = "270px")
      ),
      div(class = "col-md-7",
        tags$label("Nombre del banco", class = "form-label small fw-semibold mb-1"),
        textInput("banco_nombre", NULL, value = nom_val, width = "100%",
                  placeholder = "BanBaj\u00edo")
      ),
      div(class = "col-md-3",
        tags$label("Clave CECOBAN", class = "form-label small fw-semibold mb-1"),
        textInput("banco_clave", NULL, value = cla_val, width = "100%",
                  placeholder = "030")
      )
    ),

    # Autofill: picker value is "Nombre|clave"; JS splits and fills the two inputs
    tags$script(HTML("
      $(document).on('change', '#banco_picker', function() {
        var parts = $(this).val().split('|');
        if (parts.length === 2 && parts[0] !== '' && parts[0] !== 'Otro') {
          $('#banco_nombre').val(parts[0]);
          $('#banco_clave' ).val(parts[1]);
          Shiny.setInputValue('banco_nombre', parts[0], {priority:'event'});
          Shiny.setInputValue('banco_clave',  parts[1], {priority:'event'});
        }
      });
    ")),

    div(class = "d-flex gap-2 mt-3",
      actionButton("banco_save",
        tagList(icon("floppy-disk"), if (is_new) " Agregar banco" else " Guardar"),
        class = "btn btn-primary btn-sm"),
      if (!is_new)
        actionButton("banco_delete", tagList(icon("trash"), " Eliminar"),
                     class = "btn btn-outline-danger btn-sm") else NULL,
      actionButton("cta_cancel", "Cancelar", class = "btn btn-outline-secondary btn-sm")
    )
  )
}

# ── Account form ──────────────────────────────────────────────────────────────
.cuenta_form_ui <- function(row = NULL, bancos = NULL, pre_banco_id = NULL,
                             pre_empresa = "", cmap = COMPANY_MAP) {
  is_new <- is.null(row)

  banco_choices <- if (!is.null(bancos) && nrow(bancos)) {
    setNames(bancos$id, bancos$nombre)
  } else { c("(agrega un banco primero)" = "") }

  sel_banco   <- if (!is_new) row$banco_id %||% "" else pre_banco_id %||% ""
  emp_val     <- if (!is_new) row$Empresa  %||% "" else pre_empresa
  razon_val   <- if (!is_new) row$razon_social %||% "" else ""
  rfc_val     <- if (!is_new) row$rfc  %||% "" else ""
  moneda_val  <- if (!is_new) row$Moneda   %||% "MXN" else "MXN"
  alias_val   <- if (!is_new) row$alias    %||% "" else ""
  cuenta_val  <- if (!is_new) row$cuenta   %||% "" else ""
  clabe_val   <- if (!is_new) row$clabe_interbancaria %||% "" else ""
  default_val <- if (!is_new) isTRUE(row$is_ppl_default) else TRUE  # default TRUE for convenience

  razon_ph <- if (nzchar(emp_val) && emp_val %in% names(cmap))
    unname(cmap[emp_val]) else "Empresa Modelo SA de CV"

  div(class = "catalogo-form border rounded p-3 bg-light",
    tags$h6(class = "fw-semibold mb-3",
      if (is_new) tagList(icon("plus"), " Nueva cuenta bancaria")
      else        tagList(icon("pencil"), " Editar cuenta bancaria")),

    div(class = "row g-2",

      # Row 1: Empresa + Banco + Moneda + PPL checkbox
      div(class = "col-md-3",
        tags$label("Empresa", class = "form-label small fw-semibold mb-1"),
        selectInput("cta_empresa", NULL,
          choices  = c("\u2014 seleccionar \u2014" = "",
                       setNames(names(cmap), unname(cmap))),
          selected = emp_val, width = "100%")
      ),
      div(class = "col-md-4",
        tags$label("Banco", class = "form-label small fw-semibold mb-1"),
        selectInput("cta_banco_id", NULL,
                    choices = banco_choices, selected = sel_banco, width = "100%")
      ),
      div(class = "col-md-2",
        tags$label("Moneda", class = "form-label small fw-semibold mb-1"),
        selectInput("cta_moneda", NULL,
                    choices = c("MXN","USD","EUR"), selected = moneda_val, width = "100%")
      ),
      div(class = "col-md-3 d-flex align-items-end pb-1",
        div(class = "form-check",
          tags$input(
            type  = "checkbox", class = "form-check-input",
            id    = "cta_is_ppl_default",
            if (default_val) list(checked = "checked") else list()
          ),
          tags$label(class = "form-check-label small fw-semibold",
                     `for` = "cta_is_ppl_default",
            tagList(tags$span(class = "badge bg-primary me-1", "PPL"),
                    "usar como origen")
          )
        )
      ),

      # Row 2: Cuenta origen + CLABE interbancaria + Alias
      div(class = "col-md-4",
        tags$label(
          tagList("Cuenta origen",
            tags$span(class = "text-muted fw-normal", " \u2014 11 d\u00edgitos")),
          class = "form-label small fw-semibold mb-1"),
        textInput("cta_cuenta", NULL, value = cuenta_val, width = "100%",
                  placeholder = "08153004987")
      ),
      div(class = "col-md-5",
        tags$label(
          tagList("CLABE interbancaria",
            tags$span(class = "text-muted fw-normal", " \u2014 18 d\u00edgitos")),
          class = "form-label small fw-semibold mb-1"),
        textInput("cta_clabe", NULL, value = clabe_val, width = "100%",
                  placeholder = "030310012345678901")
      ),
      div(class = "col-md-3",
        tags$label("Alias interno", class = "form-label small fw-semibold mb-1"),
        textInput("cta_alias", NULL, value = alias_val, width = "100%",
                  placeholder = "EMP-MXN")
      ),

      # Row 3: Saldo inicial + Razón social + RFC
      div(class = "col-md-3",
        tags$label(
          tagList("Saldo inicial",
            tags$span(class = "text-muted fw-normal", " \u2014 apertura")),
          class = "form-label small fw-semibold mb-1"),
        numericInput("cta_saldo_inicial", NULL,
                     value = if (!is_new) (row$saldo_inicial %||% 0) else 0,
                     min = 0, step = 1000, width = "100%")
      ),
      div(class = "col-md-6",
        tags$label("Raz\u00f3n Social", class = "form-label small fw-semibold mb-1"),
        textInput("cta_razon_social", NULL, value = razon_val, width = "100%",
                  placeholder = razon_ph)
      ),
      div(class = "col-md-3",
        tags$label("RFC", class = "form-label small fw-semibold mb-1"),
        textInput("cta_rfc", NULL, value = rfc_val, width = "100%",
                  placeholder = "EMP850101HX1")
      )
    ),

    div(class = "small text-muted mt-2 border-top pt-2",
      icon("circle-info"),
      " La ", tags$strong("Cuenta origen"), " es el n\u00famero de 12 d\u00edgitos en el ",
      "campo origen del archivo PPL (ej: ", tags$code("08153004987"),
      "). La CLABE interbancaria es el n\u00famero de 18 d\u00edgitos para recibir transferencias SPEI."
    ),

    div(class = "d-flex gap-2 mt-3",
      actionButton("cuenta_save",
        tagList(icon("floppy-disk"), if (is_new) " Agregar cuenta" else " Guardar cambios"),
        class = "btn btn-primary btn-sm"),
      if (!is_new)
        actionButton("cuenta_delete", tagList(icon("trash"), " Desactivar"),
                     class = "btn btn-outline-danger btn-sm") else NULL,
      actionButton("cta_cancel", "Cancelar", class = "btn btn-outline-secondary btn-sm")
    )
  )
}

# =============================================================================
# Usuarios panel UI helpers
# =============================================================================
.usuarios_panel_ui <- function(usuarios_df) {
  div(
    div(class = "d-flex align-items-center gap-2 mb-3",
      tags$h6(class = "fw-semibold mb-0",
              tagList(icon("users"), " Usuarios")),
      div(class = "ms-auto",
        actionButton("usr_new", tagList(icon("plus"), " Nuevo usuario"),
                     class = "btn btn-sm btn-outline-primary")
      )
    ),
    div(style = "max-height: 220px; overflow-y: auto; margin-bottom: 1rem;",
      DT::dataTableOutput("tbl_usuarios")
    ),
    tags$hr(),
    uiOutput("usr_form_panel")
  )
}

.usuario_form_ui <- function(row = NULL) {
  is_new <- is.null(row)
  tiers  <- c(
    "Dev (control total)"     = "dev",
    "Admin"                   = "admin",
    "Finance"                 = "finance",
    "Analysis (solo lectura)" = "analysis"
  )

  div(class = "border rounded p-3 bg-light",
    tags$h6(class = "fw-semibold mb-3",
      if (is_new) tagList(icon("user-plus"), " Nuevo usuario")
      else        tagList(icon("pencil"),    " Editar usuario")
    ),
    div(class = "row g-2",
      div(class = "col-md-4",
        tags$label("Usuario", class = "form-label small fw-semibold mb-1"),
        textInput("usr_username", NULL,
                  value       = if (!is_new) row$username else "",
                  placeholder = "nombre.apellido",
                  width       = "100%")
      ),
      div(class = "col-md-4",
        tags$label("Nombre para mostrar", class = "form-label small fw-semibold mb-1"),
        textInput("usr_display", NULL,
                  value       = if (!is_new) row$display_name else "",
                  placeholder = "Nombre Completo",
                  width       = "100%")
      ),
      div(class = "col-md-4",
        tags$label("Tier", class = "form-label small fw-semibold mb-1"),
        selectInput("usr_tier", NULL,
                    choices  = tiers,
                    selected = if (!is_new) row$tier else "finance",
                    width    = "100%")
      ),
      div(class = "col-md-5",
        tags$label(
          if (is_new) "Contrase\u00f1a" else "Nueva contrase\u00f1a (dejar vac\u00edo para no cambiar)",
          class = "form-label small fw-semibold mb-1"),
        passwordInput("usr_password", NULL,
                      value = "",
                      width = "100%")
      ),
      if (!is_new) div(class = "col-md-3 d-flex align-items-end pb-1",
        div(class = "form-check",
          tags$input(type = "checkbox", class = "form-check-input",
                     id = "usr_activo",
                     if (isTRUE(row$activo)) list(checked = "checked") else list()),
          tags$label(class = "form-check-label small", `for` = "usr_activo", "Activo")
        )
      ) else NULL
    ),
    tags$div(class = "small text-muted mt-1 mb-2",
      icon("circle-info"),
      " Las contrase\u00f1as se guardan como texto por ahora. Usa contrase\u00f1as distintas a las de otros sistemas hasta implementar hashing (Roadmap \u00edtem 1)."
    ),
    div(class = "d-flex gap-2 mt-2",
      actionButton("usr_save",
        tagList(icon("floppy-disk"), if (is_new) " Crear usuario" else " Guardar cambios"),
        class = "btn btn-primary btn-sm"),
      actionButton("usr_cancel", "Cancelar",
                   class = "btn btn-outline-secondary btn-sm")
    ),
    tags$input(type = "hidden", id = "usr_editing_id",
               value = if (!is_new) row$id else "")
  )
}

# =============================================================================
# settings_usuarios_observer  (called from app.R, dev tier only)
# =============================================================================
settings_usuarios_observer <- function(input, output, session, shared) {

  # Reactive: always-fresh view of usuarios from S3
  usuarios_rv <- reactiveVal(NULL)

  .refresh_usuarios <- function() {
    usuarios_rv(auth_load_usuarios())
  }

  # Table render
  output$tbl_usuarios <- DT::renderDataTable({
    df <- usuarios_rv()
    if (is.null(df)) df <- auth_load_usuarios()
    tbl <- data.frame(
      Usuario  = df$username,
      Nombre   = df$display_name,
      Tier     = df$tier,
      Activo   = ifelse(df$activo %in% TRUE, "\u2713", "\u2013"),
      `Último acceso` = ifelse(
        is.na(df$last_login) | !nzchar(df$last_login %||% ""),
        "\u2014", as.character(df$last_login)),
      Editar   = sprintf(
        '<button class="bnc-btn-xs" onclick="Shiny.setInputValue(\'usr_edit_id\',\'%s\',{priority:\'event\'})">Editar</button>',
        df$id
      ),
      Eliminar = sprintf(
        '<button class="bnc-btn-xs btn-bnc-danger" onclick="Shiny.setInputValue(\'usr_delete_id\',\'%s\',{priority:\'event\'})"><i class=\"fa fa-trash\"></i></button>',
        df$id
      ),
      stringsAsFactors = FALSE, check.names = FALSE
    )
    DT::datatable(tbl, escape = FALSE, rownames = FALSE, selection = "none",
      options = list(dom = "t", pageLength = 20,
                     language = list(emptyTable = "Sin usuarios registrados")))
  }, server = TRUE)

  # Open panel
  observeEvent(input$stg_btn_usuarios, {
    .refresh_usuarios()
    output$settings_panel <- renderUI({
      .usuarios_panel_ui(usuarios_rv() %||% auth_load_usuarios())
    })
    output$usr_form_panel <- renderUI({ NULL })
  }, ignoreInit = TRUE)

  # New user button
  observeEvent(input$usr_new, {
    output$usr_form_panel <- renderUI({ .usuario_form_ui(NULL) })
  }, ignoreInit = TRUE)

  # Edit user button (fires from DT inline button)
  observeEvent(input$usr_edit_id, {
    uid <- input$usr_edit_id
    req(nzchar(uid %||% ""))
    df  <- usuarios_rv() %||% auth_load_usuarios()
    row <- df[df$id == uid, , drop = FALSE]
    if (!nrow(row)) return()
    output$usr_form_panel <- renderUI({ .usuario_form_ui(row) })
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Cancel
  observeEvent(input$usr_cancel, {
    output$usr_form_panel <- renderUI({ NULL })
  }, ignoreInit = TRUE)

  # Delete user (soft-delete via deleted = TRUE)
  observeEvent(input$usr_delete_id, {
    uid <- input$usr_delete_id
    req(nzchar(uid %||% ""))
    df  <- auth_load_usuarios()
    row <- df[df$id == uid, , drop = FALSE]
    if (!nrow(row)) return()

    # Guard: never allow deletion of the last active dev-tier account
    if (isTRUE(row$tier == "dev")) {
      active_devs <- sum(
        df$tier == "dev" &
        df$activo %in% TRUE &
        (is.na(df$deleted) | df$deleted != TRUE),
        na.rm = TRUE
      )
      if (active_devs <= 1L) {
        showNotification(
          "No se puede eliminar el \u00fanico usuario dev activo del sistema.",
          type = "error", duration = 5)
        return()
      }
    }

    # Soft-delete
    if (!"deleted"    %in% names(df)) df$deleted    <- FALSE
    if (!"deleted_at" %in% names(df)) df$deleted_at <- NA_character_
    df$deleted[df$id    == uid] <- TRUE
    df$deleted_at[df$id == uid] <- as.character(Sys.time())

    ok <- auth_save_usuarios(df)
    if (!isTRUE(ok)) {
      showNotification("Error al eliminar en S3.", type = "warning"); return()
    }

    visible <- df[is.na(df$deleted) | df$deleted != TRUE, , drop = FALSE]
    usuarios_rv(visible)
    output$usr_form_panel <- renderUI({ NULL })
    showNotification(
      paste0("Usuario '", row$username, "' eliminado."),
      type = "message", duration = 3)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # Save
  observeEvent(input$usr_save, {
    username <- trimws(input$usr_username %||% "")
    display  <- trimws(input$usr_display  %||% "")
    tier     <- input$usr_tier            %||% "finance"
    password <- input$usr_password        %||% ""
    edit_id  <- trimws(input$usr_editing_id %||% "")
    is_new   <- !nzchar(edit_id)

    if (!nzchar(username)) {
      showNotification("El nombre de usuario es obligatorio.", type = "warning")
      return()
    }
    if (!nzchar(display)) {
      showNotification("El nombre para mostrar es obligatorio.", type = "warning")
      return()
    }
    if (is_new && !nzchar(password)) {
      showNotification("La contrase\u00f1a es obligatoria para nuevos usuarios.", type = "warning")
      return()
    }

    df <- auth_load_usuarios()

    # Guard: cannot downgrade or deactivate the last active dev-tier account
    if (!is_new) {
      idx_chk <- which(df$id == edit_id)
      if (length(idx_chk)) {
        cur_row <- df[idx_chk, ]
        if (isTRUE(cur_row$tier == "dev") &&
            (tier != "dev" || !isTRUE(input$usr_activo))) {
          active_devs <- sum(
            df$tier == "dev" &
            df$activo %in% TRUE &
            (is.na(df$deleted) | df$deleted != TRUE),
            na.rm = TRUE
          )
          if (active_devs <= 1L) {
            showNotification(
              "No se puede desactivar o cambiar el tier del \u00fanico usuario dev activo del sistema.",
              type = "error", duration = 5)
            return()
          }
        }
      }
    }

    if (is_new) {
      if (username %in% df$username) {
        showNotification(paste0("Ya existe el usuario '", username, "'."), type = "error")
        return()
      }
      new_row <- data.frame(
        id            = uuid::UUIDgenerate(),
        username      = username,
        password_hash = password,       # plain text for now — Roadmap item 1
        display_name  = display,
        tier          = tier,
        client_id     = tolower(Sys.getenv("CLIENT_ID")),
        permisos      = "{}",
        activo        = TRUE,
        created_at    = as.character(Sys.time()),
        last_login    = NA_character_,
        stringsAsFactors = FALSE
      )
      df  <- dplyr::bind_rows(df, new_row)
      msg <- paste0("Usuario '", username, "' creado.")
    } else {
      idx <- which(df$id == edit_id)
      if (!length(idx)) {
        showNotification("Usuario no encontrado.", type = "error"); return()
      }
      df$username[idx]      <- username
      df$display_name[idx]  <- display
      df$tier[idx]          <- tier
      df$activo[idx]        <- isTRUE(input$usr_activo)
      if (nzchar(password))
        df$password_hash[idx] <- password
      msg <- paste0("Usuario '", username, "' actualizado.")
    }

    ok <- auth_save_usuarios(df)
    if (!isTRUE(ok)) {
      showNotification("Error al guardar en S3.", type = "warning"); return()
    }

    usuarios_rv(df)
    output$tbl_usuarios <- DT::renderDataTable({
      tbl <- data.frame(
        Usuario  = df$username,
        Nombre   = df$display_name,
        Tier     = df$tier,
        Activo   = ifelse(df$activo %in% TRUE, "\u2713", "\u2013"),
        `Último acceso` = ifelse(
          is.na(df$last_login) | !nzchar(df$last_login %||% ""),
          "\u2014", as.character(df$last_login)),
        Editar   = sprintf(
          '<button class="bnc-btn-xs" onclick="Shiny.setInputValue(\'usr_edit_id\',\'%s\',{priority:\'event\'})">Editar</button>',
          df$id
        ),
        stringsAsFactors = FALSE, check.names = FALSE
      )
      DT::datatable(tbl, escape = FALSE, rownames = FALSE, selection = "none",
        options = list(dom = "t", pageLength = 20,
                       language = list(emptyTable = "Sin usuarios registrados")))
    }, server = TRUE)
    output$usr_form_panel <- renderUI({ NULL })
    showNotification(msg, type = "message", duration = 3)
  }, ignoreInit = TRUE)
}