# =============================================================================
# app.R
# =============================================================================

source("R/pagar_hoy_module.R")
source("R/search_module.R")
source("R/vencidos_module.R")
source("R/treasury_map_module.R")
source("R/reporte_module.R")
source("R/tiers_module.R")
source("R/empresas_module.R")
source("R/interco_module.R")
source("R/staging_browse_module.R")
source("R/pasivos_table_pivot.R")
source("R/pasivos_table_styles.R")
source("R/pasivos_rates.R")
source("R/pasivos_wizard_validate.R")
source("R/pasivos_wizard_steps.R")
source("R/pasivos_wizard_module.R")
source("R/pasivos_edit_confirm_module.R")
source("R/pasivos_list_module.R")
source("R/pasivos_table_module.R")
# bancos_parser.R / bancos_persistence.R / bancos_module.R sourced in global.R

AR_CONFIG <- list(
  ledger       = "AR",
  title        = "Cuentas por cobrar",
  title_prefix = "Cobros esperados",
  party_label  = "Cliente",
  code_col     = "Código de cliente",
  sap_endpoint = "Invoices"
)

AP_CONFIG <- list(
  ledger       = "AP",
  title        = "Cuentas por pagar",
  title_prefix = "Pagos programados",
  party_label  = "Proveedor",
  code_col     = "Código de proveedor",
  sap_endpoint = "PurchaseInvoices"
)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- bslib::page_navbar(
  title    = "HopDesk",
  theme    = bslib::bs_theme(version = 5, bootswatch = "flatly",
                              primary = "#0A58CA"),
  fillable = TRUE,
  id       = "nav",
  header   = tagList(
    shinyjs::useShinyjs(),
    app_styles(),
    app_scripts(),
    # Hide shinymanager's default floating session/logout widget (uses .mfb-* classes)
    tags$style(HTML("[id*=shinymanager],[class*=shinymanager],[class*=mfb-]{display:none!important}")),
    # Hide GRUPO menu by default — server shows it only for dev / admin tier
    tags$style(HTML(".navbar .nav-link[data-value='GRUPO']{display:none!important}")),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        function hideSmWidgets() {
          document.querySelectorAll('[id*=\"shinymanager\"],[class*=\"shinymanager\"],[class*=\"mfb-\"]')
            .forEach(function(e){ e.style.setProperty('display','none','important'); });
        }
        hideSmWidgets();
        new MutationObserver(hideSmWidgets)
          .observe(document.body, {childList:true, subtree:true});
      });
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('updateTabLabel', function(msg) {
        var attempts = 0;
        function tryUpdate() {
          var all = document.querySelectorAll('#' + msg.tabsetId + ' [data-value]');
          var found = false;
          for (var i = 0; i < all.length; i++) {
            if (all[i].getAttribute('data-value') === msg.value) {
              all[i].textContent = msg.label;
              found = true;
              break;
            }
          }
          if (!found && attempts < 10) { attempts++; setTimeout(tryUpdate, 150); }
        }
        tryUpdate();
      });
    ")),
    tags$script(HTML("
      $(document).on('shiny:connected', function() {
        // val is optional — provided by shown.bs.tab via event.target.
        // Without it we fall back to querying the active element, checking
        // both regular nav-links and dropdown-items (bslib nav_menu children).
        function syncBar(val) {
          if (val === undefined) {
            var active = document.querySelector('.nav-link.active') ||
                         document.querySelector('.dropdown-item.active');
            val = active ? (active.getAttribute('data-value') || '') : '';
          }
          var bar     = document.querySelector('.control-bar');
          var navbar  = document.querySelector('nav.navbar');
          var rptWrap = document.getElementById('rpt-rpt_main_wrap');
          var rptFab  = document.getElementById('rpt-fab_root');
          var noBar   = val === 'PH' || val === 'RPT' || val === 'TIERS' || val === 'EMP';
          if (bar)     bar.style.display    = noBar ? 'none' : '';
          if (navbar)  navbar.style.display = (val === 'RPT') ? 'none' : '';
          if (rptWrap) rptWrap.style.display = (val === 'RPT') ? 'block' : 'none';
          if (rptFab)  rptFab.style.display  = (val === 'RPT') ? 'flex'  : 'none';
        }
        // Only react to outer navbar tab switches — inner tabsets (e.g., company
        // tabs inside Agenda de Hoy) also fire shown.bs.tab and would incorrectly
        // un-hide the control bar with their company-name data-values.
        $(document).on('shown.bs.tab', function(e) {
          if ($(e.target).closest('nav.navbar').length) {
            syncBar(e.target.getAttribute('data-value') || '');
          }
        });
        setTimeout(syncBar, 200);
        // shiny:idle fires after Shiny finishes initial processing — reliably
        // catches the case where 200ms wasn't enough on slow connections.
        $(document).one('shiny:idle', function() { syncBar(); });
      });
    ")),
    tags$script(HTML("
      // Keep-alive: ping Shiny every 90s so the WebSocket never idles out.
      // Browsers throttle background timers when the tab is hidden, but 90s
      // is short enough that even a 10x throttle stays under proxy timeouts.
      setInterval(function() {
        if (Shiny.shinyapp && Shiny.shinyapp.isConnected()) {
          Shiny.setInputValue('.__keepalive__', Math.random());
        }
      }, 90000);
    ")),
    tags$script(HTML("
      // IC code-to-name lookup — populated by server via 'icLookupData' message.
      // Keyed as: icLookup[ledger][ini][numericCode] = 'Company Name'
      window.icLookup = {};

      Shiny.addCustomMessageHandler('icLookupData', function(data) {
        window.icLookup = data;
        // Update every visible IC input immediately after lookup is loaded
        document.querySelectorAll('input[type=\"text\"]').forEach(function(inp) {
          if (/^ic_(ar|ap)_[A-Z]+_\\d+$/.test(inp.id)) _icUpdateLabel(inp);
        });
      });

      function _icUpdateLabel(input) {
        var m = input.id.match(/^ic_(ar|ap)_([A-Z]+)_(\\d+)$/);
        if (!m) return;
        var ledger = m[1], ini = m[2], slot = m[3];
        var lbl = document.getElementById('ic_lbl_' + ledger + '_' + ini + '_' + slot);
        if (!lbl) return;
        var raw  = input.value.trim().toUpperCase();
        var name = '';
        if (window.icLookup[ledger] && window.icLookup[ledger][ini]) {
          var lkpIni = window.icLookup[ledger][ini];
          // Try exact match first, then with common SAP prefixes
          name = lkpIni[raw] || lkpIni['C' + raw] || lkpIni['P' + raw] || '';
        }
        lbl.textContent = name;
        lbl.style.color = name ? '#0d6efd' : '#aaa';
      }

      // Live-update label as user types
      document.addEventListener('input', function(e) {
        if (e.target && /^ic_(ar|ap)_[A-Z]+_\\d+$/.test(e.target.id)) {
          _icUpdateLabel(e.target);
        }
      });
    ")),
    div(
      class = "control-bar border-bottom bg-white",
      div(
        class = "px-3 pt-2 pb-1 d-flex align-items-center gap-2",
        tags$span(class = "small text-muted flex-shrink-0", "Empresa:"),
        uiOutput("empresa_ui")
      ),
      tags$hr(class = "my-0 mx-3"),
      div(
        class = "px-3 pt-1 pb-2 d-flex flex-wrap gap-3 align-items-end",
        div(class = "control-item",
          tags$label("Mes", class = "form-label mb-0 small text-muted"),
          uiOutput("month_ui")
        ),
        div(class = "control-item",
          tags$label("Moneda", class = "form-label mb-0 small text-muted"),
          uiOutput("currency_ui")
        ),
        div(class = "control-item",
          tags$label("Intercompany", class = "form-label mb-0 small text-muted"),
          shinyWidgets::radioGroupButtons(
            inputId  = "ic_mode", label = NULL,
            choices  = c("Excluir" = "exclude", "Incluir" = "include", "Sólo IC" = "only"),
            selected = "exclude", size = "sm", status = "outline-secondary"
          )
        ),
        div(class = "ms-auto d-flex gap-2 align-items-end",
          actionButton("btn_refresh",  icon("rotate"), label = NULL,
                       class = "btn btn-outline-secondary btn-sm",
                       title = "Actualizar datos de SAP"),
          actionButton("btn_search",    icon("magnifying-glass"), label = NULL,
                       class = "btn btn-outline-secondary btn-sm",
                       title = "Buscar facturas del mes"),
          actionButton("btn_settings", icon("gear"), label = NULL,
             class = "btn btn-outline-secondary btn-sm",
             title = "Configuración"),
          actionButton("btn_add_entry", icon("plus"),   label = "Agregar",
                       class = "btn btn-outline-primary btn-sm")
        )
      )
    )
  ),

  bslib::nav_panel(
    title = "Cobros (CxC)", value = "AR",
    div(class = "h-100 position-relative",
      ledgerModuleUI("ar"),
      div(class = "notes-toggle-bar",
        actionLink("toggle_notes_ar",
                   tagList(icon("sticky-note"), " Notas del mes"),
                   class = "text-muted small")
      ),
      uiOutput("notes_panel_ar")
    )
  ),

  bslib::nav_panel(
    title = "Pagos (CxP)", value = "AP",
    div(class = "h-100 position-relative",
      ledgerModuleUI("ap"),
      div(class = "notes-toggle-bar",
        actionLink("toggle_notes_ap",
                   tagList(icon("sticky-note"), " Notas del mes"),
                   class = "text-muted small")
      ),
      uiOutput("notes_panel_ap")
    )
  ),

  bslib::nav_panel(
    title = tagList(
      icon("clock-rotate-left"), " Vencidos\u00a0",
      tags$span(id    = "ven_tab_badge",
                class = "badge rounded-pill bg-danger",
                style = "font-size:.6rem; vertical-align:middle; display:none;",
                "")
    ),
    value = "VEN",
    div(class = "h-100 overflow-auto",
      vencidosUI("ven")
    )
  ),

  bslib::nav_panel(
    title = tagList(icon("calendar-day"), " Agenda de Hoy"),
    value = "PH",
    pagarHoyUI("ph")
  ),

  bslib::nav_panel(
    title = tagList(icon("building-columns"), " Bancos"),
    value = "BNC",
    div(class = "h-100 overflow-auto p-0",
      bancosUI("bnc")
    )
  ),
  bslib::nav_panel(
    title = tagList(icon("arrows-left-right"), " Intercompany"),
    value = "IC",
    div(class = "h-100 overflow-auto p-0",
      intercoUI("ic")
    )
  ),
  bslib::nav_panel(
    title = tagList(icon("calendar-check"), " Pasivos"),
    value = "PSV",
    div(class = "h-100 overflow-auto",
      pasivos_table_module_ui("pt")
    )
  ),
  bslib::nav_panel(
    title = tagList(icon("file-pdf"), " Reporte"),
    value = "RPT",
    reporteUI("rpt")
  ),
  bslib::nav_menu(
    title = tagList(icon("building-user"), " Grupo"),
    value = "GRUPO",
    bslib::nav_panel(
      title = tagList(icon("users-gear"), " Usuarios"),
      value = "TIERS",
      tiersUI("trs")
    ),
    bslib::nav_panel(
      title = tagList(icon("building"), " Empresas"),
      value = "EMP",
      empresasUI("emp")
    )
  ),
  bslib::nav_spacer(),
  bslib::nav_item(
    uiOutput("navbar_user_badge")
  ),
  bslib::nav_item(
    actionButton("btn_logout",
                 label = tagList(icon("right-from-bracket"), " Salir"),
                 class = "btn btn-sm btn-outline-light",
                 style = "margin:4px 8px; font-size:.8rem; opacity:.9;")
  )
)

ui <- shinymanager::secure_app(
  ui,
  language = "es",
  tags_top = tagList(
    tags$style(HTML("
      /* ── HopDesk login skin ─────────────────────────────────── */

      /* Page background: near-black with faint angular grid */
      body {
        background: #07090d !important;
      }
      body::before {
        content: '';
        position: fixed; inset: 0; z-index: 0; pointer-events: none;
        background-image:
          linear-gradient(rgba(180,148,68,0.032) 1px, transparent 1px),
          linear-gradient(90deg, rgba(180,148,68,0.032) 1px, transparent 1px);
        background-size: 56px 56px;
      }

      /* Hide shinymanager default heading — replaced in tags_top */
      #shinymanager-content h2,
      .shinymanager-panel h2,
      h2.text-center {
        display: none !important;
      }

      /* Login card */
      #shinymanager-content .well,
      #shinymanager-content .panel,
      #shinymanager-content .card,
      .shinymanager-panel {
        background: #0c1520 !important;
        border: 1px solid rgba(180,148,68,0.2) !important;
        border-radius: 3px !important;
        box-shadow: 0 40px 100px rgba(0,0,0,0.72),
                    0 0 0 1px rgba(180,148,68,0.05) !important;
        position: relative; z-index: 1;
      }

      /* Labels */
      #shinymanager-content label {
        color: #3d5878 !important;
        font-size: 0.7rem !important;
        letter-spacing: 0.14em !important;
        text-transform: uppercase !important;
      }

      /* Text inputs */
      #shinymanager-content .form-control {
        background: #0f1b29 !important;
        border: 1px solid rgba(180,148,68,0.2) !important;
        color: #d8e4f0 !important;
        border-radius: 2px !important;
        font-size: 0.9rem !important;
        padding: 9px 12px !important;
      }
      #shinymanager-content .form-control:focus {
        border-color: rgba(180,148,68,0.55) !important;
        box-shadow: 0 0 0 3px rgba(180,148,68,0.07) !important;
        background: #122030 !important;
        color: #e8f0fa !important;
      }

      /* Login button */
      #shinymanager-content .btn-primary {
        background: #b49444 !important;
        border-color: #b49444 !important;
        color: #07090d !important;
        font-weight: 700 !important;
        letter-spacing: 0.15em !important;
        border-radius: 2px !important;
        text-transform: uppercase !important;
        font-size: 0.76rem !important;
        padding: 11px !important;
        width: 100% !important;
        margin-top: 6px !important;
      }
      #shinymanager-content .btn-primary:hover {
        background: #cba84e !important;
        border-color: #cba84e !important;
      }

      /* Divider */
      #shinymanager-content hr {
        border-color: rgba(180,148,68,0.1) !important;
        margin-top: 12px !important;
        margin-bottom: 8px !important;
      }

      /* Tighten card inner padding */
      #shinymanager-content .panel-body,
      #shinymanager-content .card-body,
      #shinymanager-content .well {
        padding-top: 10px !important;
        padding-bottom: 12px !important;
      }
    ")),
    # JS backup — hides the h3 heading and strips spacer by known id/selector
    tags$script(HTML("
      $(function() {
        var fix = function() {
          $('#auth-shinymanager-auth-head').hide();
          $('div[style*=\"height: 70px\"]').hide();
          $('.panel-auth > br').hide();
          $('.panel-auth .panel-body').css('padding-top', '4px');
        };
        fix();
        var n = 0, t = setInterval(function() {
          fix();
          if (n++ > 15) clearInterval(t);
        }, 200);
      });
    ")),
    tags$div(
      style = "text-align:center; padding: 2px 0 4px;",

      # ── Precision instrument mark (compass / astrolabe reticle) ──────────────
      HTML('
        <svg width="52" height="52" viewBox="0 0 60 60" fill="none"
             xmlns="http://www.w3.org/2000/svg"
             style="display:block; margin:0 auto 10px; opacity:0.92;">
          <!-- outer dashed ring -->
          <circle cx="30" cy="30" r="26" stroke="#b49444" stroke-width="0.7"
                  stroke-dasharray="3.2 2.6" opacity="0.6"/>
          <!-- inner ring -->
          <circle cx="30" cy="30" r="18" stroke="#b49444" stroke-width="0.45"
                  opacity="0.28"/>
          <!-- cardinal crosshair lines -->
          <line x1="30" y1="2"  x2="30" y2="9"  stroke="#b49444" stroke-width="1.1"/>
          <line x1="30" y1="51" x2="30" y2="58" stroke="#b49444" stroke-width="1.1"/>
          <line x1="2"  y1="30" x2="9"  y2="30" stroke="#b49444" stroke-width="1.1"/>
          <line x1="51" y1="30" x2="58" y2="30" stroke="#b49444" stroke-width="1.1"/>
          <!-- 45-degree secondary marks -->
          <line x1="10" y1="10" x2="14.5" y2="14.5" stroke="#b49444"
                stroke-width="0.6" opacity="0.4"/>
          <line x1="45.5" y1="45.5" x2="50" y2="50" stroke="#b49444"
                stroke-width="0.6" opacity="0.4"/>
          <line x1="50" y1="10" x2="45.5" y2="14.5" stroke="#b49444"
                stroke-width="0.6" opacity="0.4"/>
          <line x1="10" y1="50" x2="14.5" y2="45.5" stroke="#b49444"
                stroke-width="0.6" opacity="0.4"/>
          <!-- center diamond -->
          <rect x="26.5" y="26.5" width="7" height="7"
                transform="rotate(45 30 30)"
                stroke="#b49444" stroke-width="0.75"
                fill="rgba(180,148,68,0.09)"/>
          <!-- center dot -->
          <circle cx="30" cy="30" r="1.7" fill="#b49444"/>
        </svg>
      '),

      # Brand name
      tags$div(
        style = paste0("font-size:1.55rem; font-weight:700; letter-spacing:0.22em; ",
                       "color:#b49444; text-transform:uppercase;"),
        "HopDesk"
      ),
      # Tagline
      tags$div(
        style = paste0("font-size:0.67rem; color:#283f58; letter-spacing:0.28em; ",
                       "text-transform:uppercase; margin-top:4px; margin-bottom:16px;"),
        "Treasury Intelligence Platform"
      ),

      # Auth heading — replaces shinymanager's hidden h2
      tags$div(
        style = paste0("font-size:1.2rem; font-weight:300; color:#7a99b8; ",
                       "letter-spacing:0.04em; margin-bottom:2px;"),
        "Acceda a su cuenta"
      )
    )
  ),
  tags_bottom = tags$p(
    style = paste0("text-align:center; color:#1b2e40; font-size:0.7rem; ",
                   "padding-top:10px; letter-spacing:0.08em;"),
    paste0("\u00a9 ", format(Sys.Date(), "%Y"), " HopDesk")
  ),
  head_auth = tags$style(HTML("
    /* ── HopDesk login skin ───────────────────────────────────────── */

    /* Breathing room at top (replaces the removed 70px hardcoded spacer) */
    .panel-auth { padding-top: 28px !important; }

    /* Remove the hardcoded 70px spacer div and flanking br tags */
    .panel-auth > div[style*='height: 70'] { height: 0 !important; display: none !important; }
    .panel-auth > br { display: none !important; }

    /* Hide shinymanager's auth heading */
    #auth-shinymanager-auth-head { display: none !important; }

    /* Tighten the card's top padding */
    .panel-auth .panel-body { padding-top: 4px !important; padding-bottom: 14px !important; }

    /* Form labels */
    .panel-auth label {
      font-size: 0.7rem !important;
      letter-spacing: 0.12em !important;
      text-transform: uppercase !important;
      color: #6b8299 !important;
      font-weight: 400 !important;
      margin-bottom: 4px !important;
    }

    /* Text inputs */
    .panel-auth .form-control {
      border: 1px solid rgba(180,148,68,0.3) !important;
      border-radius: 2px !important;
      background: rgba(180,148,68,0.025) !important;
      color: #2a3d52 !important;
      padding: 9px 12px !important;
      box-shadow: none !important;
    }
    .panel-auth .form-control:focus {
      border-color: #b49444 !important;
      box-shadow: 0 0 0 3px rgba(180,148,68,0.1) !important;
      background: rgba(180,148,68,0.05) !important;
      outline: none !important;
    }

    /* Login button */
    .panel-auth .btn-primary {
      background: #b49444 !important;
      border-color: #b49444 !important;
      color: #fff !important;
      font-weight: 600 !important;
      letter-spacing: 0.14em !important;
      border-radius: 2px !important;
      font-size: 0.76rem !important;
      text-transform: uppercase !important;
      padding: 10px !important;
      width: 100% !important;
      margin-top: 4px !important;
    }
    .panel-auth .btn-primary:hover {
      background: #cba84e !important;
      border-color: #cba84e !important;
    }

    /* Divider */
    .panel-auth hr {
      border-color: rgba(180,148,68,0.18) !important;
      margin-top: 14px !important;
      margin-bottom: 8px !important;
    }
  ")),
  enable_admin     = FALSE,
  timeout_session  = 4000,   # ← auth inactivity timeout in seconds (tweak here)
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly", primary = "#0A58CA")
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Session diagnostics ──────────────────────────────────────────────────────
  .t_session <- proc.time()
  .GlobalEnv$.session_count <- .GlobalEnv$.session_count + 1L
  .sn <- .GlobalEnv$.session_count
  message(sprintf("[SESSION] server() #%d started at %s  (1=auth gate, 2=real app)",
                  .sn, format(Sys.time(), "%H:%M:%S")))

  # Keep-alive — absorbs the JS heartbeat ping with zero side effects
  observeEvent(input[[".__keepalive__"]], {}, ignoreInit = TRUE)

  # ── Logout button ─────────────────────────────────────────────────────────────
  observeEvent(input$btn_logout, {
    shinyjs::runjs("
      document.cookie.split(';').forEach(function(c) {
        document.cookie = c.replace(/^ +/, '').replace(/=.*/, '=;expires=' + new Date().toUTCString() + ';path=/');
      });
      location.reload();
    ")
  }, ignoreInit = TRUE)

  # Match Shiny's own session idle timeout to the instance and auth timeouts
  shiny::shinyOptions(idletimeout = 4000)   # ← Shiny session timeout in seconds

  # ── Authentication gate ──────────────────────────────────────────────────────
  res_auth <- shinymanager::secure_server(
    check_credentials = auth_check_credentials()
  )
  message(sprintf("[AUTH] secure_server() resolved at t+%.1fs (session #%d)",
                  (proc.time() - .t_session)[["elapsed"]], .sn))

  # Current authenticated user — available throughout server.
  # Uses res_auth (reactiveValues from secure_server) directly as both the
  # reactive trigger and data source. This is more reliable than get_session_info
  # which has no reactive dependency and only reflects static session state.
  current_user_info <- reactive({
    user_val <- tryCatch(res_auth$user, error = function(e) NULL)
    tier_val <- tryCatch(res_auth$tier, error = function(e) NULL)
    name_val <- tryCatch(res_auth$name, error = function(e) NULL)
    if (is.null(user_val) || !nzchar(user_val %||% ""))
      return(list(user = "unknown", name = "unknown", tier = "finance"))
    list(
      user = user_val %||% "unknown",
      name = name_val %||% user_val %||% "unknown",
      tier = tier_val %||% "finance"
    )
  })

  # ── Shared state ────────────────────────────────────────────────────────────
  # All auxiliary objects are pre-loaded in global.R at process start.
  # Reading from .app_data_cache is instant (in-memory) vs ~400ms per S3 call.
  sap_data          <- reactiveVal(list(AR = NULL, AP = NULL))
  moves_db              <- reactiveVal(NULL)
  policy_moves_db       <- reactiveVal(NULL)
  policy_catalog_db     <- reactiveVal(NULL)
  partner_policies_db   <- reactiveVal(NULL)
  holiday_overrides_db  <- reactiveVal(NULL)
  notes_df          <- reactiveVal(NULL)
  tags_db           <- reactiveVal(NULL)
  manual_inv        <- reactiveVal(NULL)
  sap_ov_db         <- reactiveVal(NULL)
  interco_v2        <- reactiveVal(list(ar_prefix = "C", ap_prefix = "P", companies = list()))
  pagar_hoy_db      <- reactiveVal(NULL)
  abonos_db         <- reactiveVal(NULL)
  proveedores_db    <- reactiveVal(NULL)
  parte_alias_map_db <- reactiveVal(
    tryCatch(load_parte_alias_map(), error = function(e) tibble::tibble(
      Parte = character(), Empresa = character(), alias = character(),
      linked_by = character(), linked_at = character()
    ))
  )
  proveedores_inactivos_db <- reactiveVal(NULL)
  papelera_rv           <- reactiveVal(NULL)
  audit_mode            <- reactiveVal(FALSE)
  active_entry_ledger   <- reactiveVal(NULL)
  sap_loading       <- reactiveVal(FALSE)
  sap_snapshot_info <- reactiveVal(list(AR = NULL, AP = NULL))
  search_raw_data   <- reactiveVal(NULL)
  current_user      <- reactive({
    info <- tryCatch(shinymanager::get_session_info(session), error = function(e) NULL)
    if (!is.null(info) && nzchar(info$user %||% "")) info$user
    else session$user %||% "anon"
  })
  ctas_cuentas      <- reactiveVal(NULL)
  # Bancos module reactive vals
  bancos_movimientos_db <- reactiveVal(NULL)
  bancos_cuentas_db     <- reactiveVal(NULL)
  bancos_confirmados_db <- reactiveVal(NULL)
  conciliacion_rv       <- reactiveVal(NULL)
  .s3_load_complete     <- reactiveVal(FALSE)
  .phase1_done          <- reactiveVal(FALSE)

  # ── Empresa data — live source from empresas.rds ─────────────────────────────
  # Populated by empresasServer after load/save; falls back to static COMPANY_MAP.
  empresas_db <- reactiveVal(NULL)

  # Named vector: initials → nombre_corto for active, non-deleted companies.
  # Falls back to the static COMPANY_MAP until empresas.rds loads.
  company_map_rv <- reactive({
    df <- empresas_db()
    if (is.null(df) || !nrow(df)) return(COMPANY_MAP)
    active <- df[
      (is.na(df$deleted) | df$deleted != TRUE) &
      (isTRUE(df$activa) | df$activa == TRUE), , drop = FALSE]
    if (!nrow(active)) return(COMPANY_MAP)
    setNames(active$nombre_corto, active$initials)
  })

  # ── Empresa toggle state ─────────────────────────────────────────────────────
  # Initialises with COMPANY_MAP; refreshes to full selection when empresas loads.
  empresa_sel_rv <- reactiveVal(unname(COMPANY_MAP))

  # When empresas data arrives, keep COMPANY_MAP in sync and preserve the user's
  # current filter — only auto-add empresas they haven't seen before.
  observeEvent(empresas_db(), {
    cmap     <- company_map_rv()
    all_now  <- unname(cmap)
    cur_sel  <- empresa_sel_rv()
    # Track which full names were present last time so we can detect new arrivals.
    prev       <- isolate(.GlobalEnv$.prev_company_names %||% character(0))
    new_cos    <- setdiff(all_now, prev)
    keep_sel   <- intersect(cur_sel, all_now)
    # If the user had a non-empty selection, preserve it and append any new empresas.
    # If somehow empty (first load or all empresas removed), default to all.
    new_sel    <- if (length(keep_sel)) union(keep_sel, new_cos) else all_now
    empresa_sel_rv(new_sel)
    assign(".prev_company_names", all_now, envir = .GlobalEnv)
    # Keep global COMPANY_MAP in sync so modules that read it statically
    # (settings forms, reports, etc.) see the latest companies when they render.
    assign("COMPANY_MAP", cmap, envir = .GlobalEnv)
  }, ignoreNULL = TRUE)

  # Single handler for all empresa toggle buttons — works for both static and
  # dynamic company lists. Buttons fire input$emp_btn_clicked with the initials.
  observeEvent(input$emp_btn_clicked, {
    initials <- input$emp_btn_clicked
    cmap     <- company_map_rv()
    nombre   <- cmap[[initials]]
    if (!is.null(nombre)) {
      cur <- empresa_sel_rv()
      empresa_sel_rv(if (nombre %in% cur) setdiff(cur, nombre) else union(cur, nombre))
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  currency_choices <- list(
    AR = reactiveVal(c("MXN", "USD")),
    AP = reactiveVal(c("MXN", "USD"))
  )

  ic_map_target <- reactiveVal(NULL)

  pasivos_provisions_db <- reactiveVal(
    tryCatch(load_pasivos_provisions(), error = function(e) .schema_pasivos_provision())
  )

  pasivos_liabilities_db <- reactiveVal(
    tryCatch(load_pasivos_liabilities(), error = function(e) .schema_pasivos_liability())
  )

  shared <- list(
    sap_data          = sap_data,
    sap_loading       = sap_loading,
    moves_db             = moves_db,
    policy_moves_db      = policy_moves_db,
    policy_catalog_db    = policy_catalog_db,
    partner_policies_db  = partner_policies_db,
    holiday_overrides_db = holiday_overrides_db,
    notes_df          = notes_df,
    tags_db           = tags_db,
    manual_inv        = manual_inv,
    sap_ov_db         = sap_ov_db,
    interco_v2        = interco_v2,
    pagar_hoy_db      = pagar_hoy_db,
    abonos_db         = abonos_db,
    proveedores_db    = proveedores_db,
    proveedores_inactivos_db = proveedores_inactivos_db,
    parte_alias_map_db     = parte_alias_map_db,
    papelera_rv       = papelera_rv,
    sap_snapshot_info = sap_snapshot_info,
    audit_mode          = audit_mode,
    active_entry_ledger = active_entry_ledger,
    current_user        = current_user,
    current_user_info   = current_user_info,
    amount_col        = reactive("Saldo vencido"),
    empresa_sel       = empresa_sel_rv,
    empresas_db       = empresas_db,
    company_map       = company_map_rv,
    ctas_cuentas           = ctas_cuentas,
    bancos_movimientos     = bancos_movimientos_db,
    bancos_cuentas         = bancos_cuentas_db,
    bancos_confirmados     = bancos_confirmados_db,
    conciliacion_rv        = conciliacion_rv,
    pasivos_provisions_db  = pasivos_provisions_db,
    pasivos_liabilities_db = pasivos_liabilities_db,
    ic_map_target          = ic_map_target,
    currency_choices  = currency_choices,
    month_val         = list(
      AR = reactive(input$month_sel %||% Sys.Date()),
      AP = reactive(input$month_sel %||% Sys.Date())
    ),
    currency = list(
      AR = reactive(input$cur_ar),
      AP = reactive(input$cur_ap)
    ),
    ic_mode = list(
      AR = reactive(input$ic_mode %||% "exclude"),
      AP = reactive(input$ic_mode %||% "exclude")
    )
  )

  # ── Real-time agenda sync polling ────────────────────────────────────────────
  # Checks .GlobalEnv$.agenda_sync$version every 3 s; if changed, pulls updated
  # data into this session's pagar_hoy_db without hitting S3.
  .sync_clock  <- reactiveTimer(3000)
  .sync_ver_rv <- reactiveVal(NA_character_)

  observe({
    .sync_clock()
    if (!isTRUE(.GlobalEnv$.agenda_sync$is_on)) return()
    v <- tryCatch(.GlobalEnv$.agenda_sync$version, error = function(e) NA_character_)
    if (isTRUE(identical(v, .sync_ver_rv()))) return()
    d <- tryCatch(.GlobalEnv$.agenda_sync$data, error = function(e) NULL)
    if (!is.null(d)) {
      pagar_hoy_db(d)
      # Also refresh bancos_confirmados so the Pasivos observer wakes up cross-session
      bc <- tryCatch(load_bancos_confirmados(), error = function(e) NULL)
      if (!is.null(bc)) bancos_confirmados_db(bc)
      .sync_ver_rv(v)
    }
  })

  # ── Abono parcial ─────────────────────────────────────────────────────────────
  setup_abono_browse(input, output, session,
                     sap_data, abonos_db, pagar_hoy_db, current_user)

  # ── Pasivos lifecycle observers and module ────────────────────────────────────
  setup_pasivos_observers(input, output, session, shared)
  setup_pasivos_module(input, output, session, shared)
  pasivos_table_module_server("pt", shared)

  # ── Stage 4: wizard, edit-confirm ─────────────────────────────────────────
  setup_pasivos_wizard(input, output, session, shared)
  setup_pasivos_edit_confirm(input, output, session, shared)

  # ── Show GRUPO menu and control per-panel visibility ─────────────────────────
  # GRUPO toggle is hidden by default via CSS (display:none!important).
  # Empresas  → dev + admin  (can_manage_empresas permission, TRUE for both by default).
  # Usuarios  → dev only; hidden from admin's dropdown so they don't see the option.
  observeEvent(shared$current_user_info(), {
    tier <- tryCatch(shared$current_user_info()$tier, error = function(e) "")
    if (!tier %in% c("dev", "admin")) return()

    # Reveal the GRUPO dropdown toggle (overrides CSS !important)
    shinyjs::runjs("
      document.querySelectorAll('.navbar .nav-link[data-value=\"GRUPO\"]')
        .forEach(function(el) {
          el.style.setProperty('display', 'flex', 'important');
          var li = el.closest('li');
          if (li) li.style.setProperty('display', 'list-item', 'important');
        });
    ")

    if (tier == "dev") {
      # Dev sees both dropdown items — ensure Usuarios item is visible
      shinyjs::runjs("
        document.querySelectorAll('.dropdown-menu a[data-value=\"TIERS\"]')
          .forEach(function(el) {
            var li = el.closest('li');
            if (li) li.style.removeProperty('display');
            else    el.style.removeProperty('display');
          });
      ")
    } else {
      # Admin: hide Usuarios item — they only access Empresas
      shinyjs::runjs("
        document.querySelectorAll('.dropdown-menu a[data-value=\"TIERS\"]')
          .forEach(function(el) {
            var li = el.closest('li');
            if (li) li.style.setProperty('display', 'none', 'important');
            else    el.style.setProperty('display', 'none', 'important');
          });
      ")
    }
  }, ignoreInit = FALSE)

  # ── Username badge in navbar ──────────────────────────────────────────────────
  output$navbar_user_badge <- renderUI({
    info <- shared$current_user_info()
    user <- info$user %||% ""
    if (!nzchar(user) || user == "unknown") return(NULL)
    tier <- info$tier %||% "finance"
    tier_color <- switch(tier,
      dev      = "#0d1b3e",
      admin    = "#6610f2",
      finance  = "#0a58ca",
      analysis = "#6c757d",
      "#6c757d"
    )
    tags$span(
      style = "display:flex; align-items:center; gap:6px; margin:4px 4px 4px 0; font-size:.8rem; opacity:.9; color:#fff;",
      icon("user-circle"),
      tags$span(user),
      tags$span(
        tier,
        style = sprintf(
          "font-size:.65rem; font-weight:700; letter-spacing:1px; text-transform:uppercase; background:%s; border:1px solid rgba(255,255,255,.35); padding:2px 7px; border-radius:20px;",
          tier_color
        )
      )
    )
  })

  # ── Diagnostic: log every sap_data() write ───────────────────────────────────
  # Fires whenever sap_data() is written (snapshot seed, cache hit, live fetch).
  # Two writes in the same session = unnecessary calendar double-render.
  observe({
    d       <- sap_data()
    ar_rows <- if (!is.null(d$AR)) nrow(d$AR) else 0L
    ap_rows <- if (!is.null(d$AP)) nrow(d$AP) else 0L
    t_now   <- proc.time()
    since_armed <- sprintf("  [+%.1fs since sap armed]",
                           (t_now - .t_sap_armed)[["elapsed"]])
    prev_hoff <- .GlobalEnv$.t_last_curchoices_done
    since_handoff <- if (!is.null(prev_hoff))
      sprintf("  [+%.1fs since %s handoff]",
              (t_now - prev_hoff$t)[["elapsed"]], prev_hoff$ledger)
    else ""
    message(sprintf("[SAP_DATA] written at t+%.1fs — AR=%d rows  AP=%d rows (session #%d)%s%s",
                    (t_now - .t_session)[["elapsed"]], ar_rows, ap_rows, .sn,
                    since_armed, since_handoff))
    # Stamp so CAL_HTML can report how long it took from this write to render.
    .GlobalEnv$.t_last_sap_data_write <- t_now
  })

  # ── SAP loader ───────────────────────────────────────────────────────────────
  # .sap_running:   mutex prevents double-runs (autoInvalidate fires while SAP
  #                 is already mid-fetch after a manual refresh)
  # .sap_ever_done: prevents autoInvalidate from immediately re-running on
  #                 the same session that just loaded
  .sap_running    <- FALSE
  .sap_ever_done  <- FALSE
  .sap_triggered  <- FALSE

  load_sap_data <- function(force = FALSE) {
    message(sprintf("[SAP] load_sap_data() starting at t+%.1fs (session #%d)",
                    (proc.time() - .t_session)[["elapsed"]], .sn))
    if (.sap_running)               return(invisible(NULL))
    if (.sap_ever_done && !force)   return(invisible(NULL))

    .sap_running <<- TRUE
    sap_loading(TRUE)
    showNotification("Conectando a SAP…", id = "sap_load",
                     duration = NULL, type = "message")
    on.exit({
      .sap_running   <<- FALSE
      sap_loading(FALSE)
      removeNotification("sap_load")
    })

    load_ledger <- function(ledger_name) {
      result <- tryCatch(fetch_all_companies(ledger_name, isolate(company_map_rv())), error = function(e) {
        showNotification(paste("SAP", ledger_name, ":", e$message), type = "warning")
        NULL
      })
      if (!is.null(result)) {
        info <- sap_snapshot_info()
        info[[ledger_name]] <- NULL
        sap_snapshot_info(info)
        return(result)
      }
      # SAP failed — reuse already-seeded snapshot data (no second S3 read)
      cur <- sap_data()[[ledger_name]]
      if (!is.null(cur)) {
        message("[SAP] ", ledger_name, " unavailable — keeping seeded snapshot")
        return(cur)
      }
      # Nothing seeded yet — last resort S3 read
      snap <- tryCatch(load_sap_snapshot(ledger_name), error = function(e) NULL)
      if (!is.null(snap)) {
        lbl <- format(snap$saved_at, "%d %b %Y %H:%M", tz = (input$client_tz %||% "America/Mexico_City"))
        showNotification(
          paste0("SAP no disponible — ", ledger_name, " mostrando datos del ", lbl),
          type = "warning", duration = 8
        )
        info <- sap_snapshot_info()
        info[[ledger_name]] <- snap$saved_at
        sap_snapshot_info(info)
        return(snap$data)
      }
      NULL
    }

    t0_ar <- proc.time()
    message(sprintf("[SAP] Fetching AR from SAP at t+%.1fs (session #%d)",
                    (proc.time() - .t_session)[["elapsed"]], .sn))
    ar <- load_ledger("AR")
    message(sprintf("[SAP] AR done in %.1fs — starting AP (session #%d)",
                    (proc.time() - t0_ar)[["elapsed"]], .sn))
    t0_ap <- proc.time()
    ap <- load_ledger("AP")
    message(sprintf("[SAP] AP done in %.1fs — total fetch %.1fs (session #%d)",
                    (proc.time() - t0_ap)[["elapsed"]],
                    (proc.time() - t0_ar)[["elapsed"]], .sn))
    .sap_ever_done <<- TRUE

    # Only write sap_data() if something actually changed.
    # If SAP failed for both ledgers and we're returning the same snapshot
    # data already seeded in .data_loaded, writing again triggers a full
    # calendar re-render for nothing.
    current    <- sap_data()
    ar_changed <- !identical(current$AR, ar)
    ap_changed <- !identical(current$AP, ap)
    message(sprintf("[SAP] identical() check — ar_changed=%s  ap_changed=%s (session #%d)",
                    ar_changed, ap_changed, .sn))
    if (ar_changed || ap_changed) {
      sap_data(list(AR = ar, AP = ap))
      if (!is.null(ar) || !is.null(ap))
        showNotification("Datos actualizados.", type = "message", duration = 3)
    }
    # Always cache at process level — even when data didn't change.
    # This is the only way session #2 knows session #1 already ran the fetch.
    # ar/ap here are either live data or the fallback snapshot — either way
    # session #2 should use them rather than re-fetching.
    .GlobalEnv$.sap_global_cache <- list(
      AR         = ar,
      AP         = ap,
      fetched_at = Sys.time()
    )
    message(sprintf("[SAP] Cross-session cache written at t+%.1fs (session #%d)",
                    (proc.time() - .t_session)[["elapsed"]], .sn))
  }

  # ── Phase 1: SAP snapshot seed (fast path) ───────────────────────────────────
  # Fires once on the first Shiny tick after the browser connects.
  # Loads ONLY the two SAP snapshots (AR + AP) and seeds sap_data() immediately.
  # Setting .s3_load_complete(TRUE) + .phase1_done(TRUE) allows:
  #   - CUR_CHOICES (priority=1) and calendar renderUIs (priority=0) to proceed
  #   - SAP dispatch observer (priority=-1) to fire
  #   - Phase 2 (priority=-2) to load aux data AFTER the UI is already rendering
  .data_loaded <- FALSE
  observe({
    if (.data_loaded) return()
    .data_loaded <<- TRUE
    # Session #1 is shinymanager's auth gate — it never needs app data and
    # its event loop must stay free so the login form renders immediately.
    # Skipping here also prevents the SAP dispatch from running in session #1
    # (which would block login button clicks until the SAP fetch completes).
    if (.sn <= 1L) {
      message(sprintf("[LOAD] Auth gate session #%d — skipping data load", .sn))
      return()
    }
    isolate({
      t0 <- proc.time()
      message(sprintf("[LOAD] Phase 1 start at t+%.1fs (session #%d)",
                      (proc.time() - .t_session)[["elapsed"]], .sn))

      # Load only the two SAP snapshot files — typically < 2s each
      t0_snap <- proc.time()
      ar_snap <- tryCatch(load_sap_snapshot("AR"), error = function(e) NULL)
      ap_snap <- tryCatch(load_sap_snapshot("AP"), error = function(e) NULL)
      message(sprintf("[LOAD] SAP snapshots fetched in %.1fs",
                      (proc.time() - t0_snap)[["elapsed"]]))

      if (!is.null(ar_snap) || !is.null(ap_snap)) {
        sap_data(list(
          AR = if (!is.null(ar_snap)) ar_snap$data else NULL,
          AP = if (!is.null(ap_snap)) ap_snap$data else NULL
        ))
        sap_snapshot_info(list(
          AR = if (!is.null(ar_snap)) ar_snap$saved_at else NULL,
          AP = if (!is.null(ap_snap)) ap_snap$saved_at else NULL
        ))
      }

      message(sprintf(
        "[LOAD] Phase 1 done in %.1fs — handing off to CUR_CHOICES (p1) + SAP dispatch (p-1), Phase 2 queued (p-2)",
        (proc.time() - t0)[["elapsed"]]
      ))
      .s3_load_complete(TRUE)
      .phase1_done(TRUE)
    })
  })

  # ── Phase 2: aux data loader (low-priority) ───────────────────────────────────
  # Triggered by .phase1_done(). Runs at priority=-2 so CUR_CHOICES (p1),
  # calendar renderUIs (p0), and SAP dispatch (p-1) all execute first.
  # Batch-fetches all deferred S3 keys into .s3_preload_cache, then loads the
  # 14 aux reactive vals. All load_*() calls are guaranteed cache hits after fetch.
  observe({
    req(.phase1_done())
    isolate({
      t0 <- proc.time()
      message(sprintf("[LOAD] Phase 2 start at t+%.1fs (session #%d)",
                      (proc.time() - .t_session)[["elapsed"]], .sn))

      # Batch-fetch all deferred S3 keys not already in the preload cache
      local({
        t0_fetch <- proc.time()
        message("[LOAD] Fetching deferred S3 objects...")
        # keys_needed: logical names whose file-suffix keys are not yet in cache.
        # Cache keyed by file suffix (e.g. "interco_v2.rds") to match .s3_read().
        keys_needed <- names(S3_KEYS)[
          !vapply(unname(S3_KEYS), exists,
                  logical(1), envir = .s3_preload_cache, inherits = FALSE)
        ]
        if (length(keys_needed)) {
          cid_lo <- tolower(Sys.getenv("CLIENT_ID"))
          bucket  <- Sys.getenv(paste0(toupper(Sys.getenv("CLIENT_ID")), "_S3_BUCKET"))
          raw <- lapply(S3_KEYS[keys_needed], function(key_suffix) {
            tryCatch({
              out <- NULL
              suppressMessages(
                capture.output(
                  out <- aws.s3::s3readRDS(
                    object = paste0(cid_lo, "/", key_suffix),
                    bucket = bucket
                  ),
                  type = "output"
                )
              )
              out
            }, error = function(e) NULL)
          })
          for (i in seq_along(keys_needed)) {
            # Store under file suffix so .s3_read() gets a cache hit
            assign(S3_KEYS[[keys_needed[i]]], raw[[i]], envir = .s3_preload_cache)
          }
          message(sprintf("[LOAD] %d deferred objects fetched in %.1fs",
                          length(keys_needed),
                          (proc.time() - t0_fetch)[["elapsed"]]))
        } else {
          message("[LOAD] All S3 keys already cached")
        }
      })

      # Load aux objects with per-item timing (all guaranteed cache hits above)
      t1 <- proc.time(); moves_db(tryCatch(load_moves(), error = function(e) NULL))
      message(sprintf("[LOAD]   moves            %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); policy_moves_db(tryCatch(load_policy_moves(), error = function(e) NULL))
      message(sprintf("[LOAD]   policy_moves     %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); policy_catalog_db(tryCatch(load_policy_catalog(), error = function(e) NULL))
      message(sprintf("[LOAD]   policy_catalog   %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); partner_policies_db(tryCatch(load_partner_policies(), error = function(e) NULL))
      message(sprintf("[LOAD]   partner_policies %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); holiday_overrides_db(tryCatch(load_holiday_overrides(), error = function(e) NULL))
      message(sprintf("[LOAD]   holiday_overrides %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); notes_df(tryCatch(load_notes(), error = function(e) NULL))
      message(sprintf("[LOAD]   notes            %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); tags_db(tryCatch(load_tags(), error = function(e) NULL))
      message(sprintf("[LOAD]   tags             %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); manual_inv(tryCatch(load_manual(), error = function(e) NULL))
      message(sprintf("[LOAD]   manual           %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); sap_ov_db(tryCatch(load_sap_overrides(), error = function(e) NULL))
      message(sprintf("[LOAD]   sap_overrides    %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      interco_v2(tryCatch(load_interco_v2(), error = function(e) NULL) %||%
                   list(ar_prefix = "C", ap_prefix = "P", companies = list()))
      message(sprintf("[LOAD]   interco_v2       %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      local({
        cfg <- tryCatch(load_agenda_sync_config(),
                        error = function(e) list(is_enabled = FALSE, .missing = TRUE))

        # Helper: merge any items the user added before Phase 2 finished loading
        # (e.g., from Agregar immediately after login). Takes the S3 data as
        # canonical and appends any in-memory-only rows not yet in S3 (by id).
        .merge_early_adds <- function(s3_ph) {
          early <- tryCatch(pagar_hoy_db(), error = function(e) NULL)
          if (is.null(early) || !nrow(early)) return(s3_ph)
          extra <- dplyr::anti_join(early, s3_ph, by = "id")
          if (nrow(extra)) {
            message(sprintf("[LOAD] pagar_hoy: merging %d item(s) added before Phase 2", nrow(extra)))
            dplyr::bind_rows(s3_ph, extra)
          } else s3_ph
        }

        if (isTRUE(cfg$.missing)) {
          # First deploy: migrate existing shared pagar_hoy.rds → sync file, enable sync
          message("[LOAD] agenda_sync: first run — migrating pagar_hoy.rds to sync")
          ph <- tryCatch(load_pagar_hoy(), error = function(e) .schema_pagar_hoy())
          ph <- .merge_early_adds(ph)
          tryCatch(save_pagar_hoy_sync(ph), error = function(e)
            message("[LOAD] agenda_sync migrate write failed: ", e$message))
          tryCatch(save_agenda_sync_config(TRUE, "system"), error = function(e) NULL)
          .GlobalEnv$.agenda_sync$is_on   <- TRUE
          .GlobalEnv$.agenda_sync$data    <- ph
          .GlobalEnv$.agenda_sync$version <- as.character(Sys.time())
          pagar_hoy_db(ph)
        } else if (isTRUE(cfg$is_enabled)) {
          .GlobalEnv$.agenda_sync$is_on <- TRUE
          ph <- if (!is.null(.GlobalEnv$.agenda_sync$data)) {
            .GlobalEnv$.agenda_sync$data
          } else {
            d <- tryCatch(load_pagar_hoy_sync(), error = function(e) .schema_pagar_hoy())
            .GlobalEnv$.agenda_sync$data    <- d
            .GlobalEnv$.agenda_sync$version <- as.character(Sys.time())
            d
          }
          ph <- .merge_early_adds(ph)
          if (!identical(ph, .GlobalEnv$.agenda_sync$data)) {
            # Early-add items were merged in — persist immediately
            tryCatch(save_pagar_hoy(ph, current_user()), error = function(e) NULL)
          }
          pagar_hoy_db(ph)
        } else {
          .GlobalEnv$.agenda_sync$is_on <- FALSE
          ph <- tryCatch(
            load_pagar_hoy_user(current_user()),
            error = function(e) tryCatch(load_pagar_hoy(), error = function(e2) NULL))
          ph <- .merge_early_adds(ph %||% .schema_pagar_hoy())
          pagar_hoy_db(ph)
        }
      })
      message(sprintf("[LOAD]   pagar_hoy        %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); abonos_db(tryCatch(load_abonos(), error = function(e) NULL))
      message(sprintf("[LOAD]   abonos           %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); papelera_rv(tryCatch(load_papelera(), error = function(e) NULL))
      message(sprintf("[LOAD]   papelera         %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); proveedores_db(tryCatch(load_proveedores(), error = function(e) NULL))
      message(sprintf("[LOAD]   proveedores      %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      proveedores_inactivos_db(tryCatch(load_proveedores_inactivos(), error = function(e) NULL))
      message(sprintf("[LOAD]   prov_inactivos   %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); ctas_cuentas(tryCatch(load_ctas_cuentas(), error = function(e) NULL))
      message(sprintf("[LOAD]   ctas_cuentas     %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      bancos_cuentas_db(tryCatch(load_bancos_cuentas(), error = function(e) NULL))
      message(sprintf("[LOAD]   bancos_cuentas   %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      bancos_movimientos_db(tryCatch(
        load_bancos_movimientos(include_deleted = TRUE), error = function(e) NULL))
      message(sprintf("[LOAD]   bancos_movs      %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      bancos_confirmados_db(tryCatch(load_bancos_confirmados(), error = function(e) NULL))
      message(sprintf("[LOAD]   bancos_confirm   %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); conciliacion_rv(tryCatch(load_conciliacion(), error = function(e) NULL))
      message(sprintf("[LOAD]   conciliacion     %.1fs", (proc.time() - t1)[["elapsed"]]))

      # Populate global cache so save_*() callbacks update it
      .cache_set("moves",        moves_db())
      .cache_set("notes",        notes_df())
      .cache_set("tags",         tags_db())
      .cache_set("manual",       manual_inv())
      .cache_set("sap_overrides", sap_ov_db())
      .cache_set("interco_v2",   interco_v2())
      .cache_set("pagar_hoy",    pagar_hoy_db())
      .cache_set("abonos",       abonos_db())
      .cache_set("papelera",     papelera_rv())
      .cache_set("proveedores",  proveedores_db())
      .cache_set("ctas_cuentas", ctas_cuentas())
      .cache_set("conciliacion", conciliacion_rv())

      message(sprintf("[LOAD] Phase 2 done in %.1fs at t+%.1fs (session #%d)",
                      (proc.time() - t0)[["elapsed"]],
                      (proc.time() - .t_session)[["elapsed"]], .sn))
    })
  }, priority = -2)

  .t_sap_armed <- proc.time()   # used by SAP_DATA observer to report the gap
  if (.sn <= 1L) {
    message(sprintf("[SAP] sap_trigger armed at t+%.1fs — inactive (auth gate, session #%d)",
                    (proc.time() - .t_session)[["elapsed"]], .sn))
  } else {
    message(sprintf("[SAP] sap_trigger armed at t+%.1fs — fires in 10s (fallback) or on Phase 1 completion (session #%d)",
                    (proc.time() - .t_session)[["elapsed"]], .sn))
  }
  sap_trigger <- reactiveTimer(10000)   # fallback: Phase 1 completes in ~1s normally

  # Heartbeat: print progress every 5s while waiting for S3 load / sap_trigger.
  # Silences once .sap_triggered flips TRUE so it doesn't run during SAP fetch.
  # No-op for session #1 — auth gate skips all data loading so .sap_triggered
  # never becomes TRUE there; without this guard the heartbeat fires forever.
  .hb_timer <- reactiveTimer(5000)
  observe({
    if (.sn <= 1L || .sap_triggered) return()
    .hb_timer()
    message(sprintf("[WAIT] t+%.1fs — S3 loading, SAP trigger pending (session #%d)",
                    (proc.time() - .t_session)[["elapsed"]], .sn))
  })

  # Shared SAP dispatch — called from both the primary and fallback observers.
  # Runs at most once per session (guarded by .sap_triggered).
  .dispatch_sap <- function() {
    if (.sap_triggered) return()
    .sap_triggered <<- TRUE
    message(sprintf("[SAP] sap_trigger fired at t+%.1fs (session #%d)",
                    (proc.time() - .t_session)[["elapsed"]], .sn))
    cache <- .GlobalEnv$.sap_global_cache
    age   <- if (!is.null(cache$fetched_at))
               as.numeric(difftime(Sys.time(), cache$fetched_at, units = "secs"))
             else Inf
    if (age < 300 && (!is.null(cache$AR) || !is.null(cache$AP))) {
      message("[SAP] Cross-session cache hit (", round(age), "s old) — skipping live fetch")
      if (!identical(sap_data()$AR, cache$AR) || !identical(sap_data()$AP, cache$AP))
        sap_data(list(AR = cache$AR, AP = cache$AP))
      .sap_ever_done <<- TRUE
      return()
    }
    message(sprintf("[SAP] No cross-session cache (age=%.0fs) — proceeding to live fetch (session #%d)",
                    age, .sn))
    load_sap_data()
  }

  # Primary: fires immediately when S3 batch load signals completion.
  # req() suspends the observer until .s3_load_complete() is TRUE —
  # prevents the initial-flush race where sap_trigger() returns Sys.time()
  # (not NULL) and the old single-observer guard fired too early.
  #
  # priority = -1: runs AFTER all default-priority (0) observers and outputs,
  # meaning both CUR_CHOICES observers and both calendar renderUI outputs finish
  # on snapshot data before load_sap_data() blocks the event loop.
  # Without this, the primary observe competed with CUR_CHOICES at priority 0,
  # won the scheduler race, and blocked AP CUR_CHOICES for 45-52s.
  observe({
    req(.s3_load_complete())
    message(sprintf(
      "[SAP] Dispatching at t+%.1fs (priority -1) — snapshot UI already rendered (session #%d)",
      (proc.time() - .t_session)[["elapsed"]], .sn))
    isolate(.dispatch_sap())
  }, priority = -1)

  # Fallback: fires at t+35s if .s3_load_complete never fires (e.g. .data_loaded errors).
  # ignoreInit=TRUE prevents this from running on the initial flush.
  # Same priority = -1 so it also yields to CUR_CHOICES and calendar renders.
  # Session #1 guard: .s3_load_complete() is never set TRUE for the auth gate,
  # so the primary observer never fires — but the 35s timer would still trigger
  # this fallback and start a blocking SAP fetch inside the auth session.
  observeEvent(sap_trigger(), {
    if (.sn <= 1L) return()
    isolate(.dispatch_sap())
  }, once = TRUE, ignoreInit = TRUE, priority = -1)

  # Fetch current SOFR and TIIE28 and write them to the forecasting store.
  # Called on startup and on every manual refresh so provision interest calculations
  # always reflect the latest published rates.
  .refresh_variable_rates <- function() {
    jobs <- list(list(fn = .fetch_sofr,   metric = "sofr"),
                 list(fn = .fetch_tiie28, metric = "tiie28"))
    for (job in jobs) {
      res <- tryCatch(job$fn(), error = function(e) list(error = conditionMessage(e)))
      rate <- res$today %||% NA_real_
      if (!is.null(res$error) || is.na(rate)) next
      tryCatch(
        forecasting_set_estimate(job$metric, Sys.Date(), rate,
                                 source_method = "auto_refresh"),
        error = function(e) NULL
      )
    }
  }

  # Fire once after first render so startup rate fetch doesn't block the UI
  session$onFlushed(function() .refresh_variable_rates(), once = TRUE)

  observeEvent(input$btn_refresh, {
    removeModal()
    .refresh_variable_rates()
    load_sap_data(force = TRUE)
  })
  autoInvalidate <- reactiveTimer(30 * 60 * 1000)
  observeEvent(autoInvalidate(), { load_sap_data() })

  # ── Search modal ─────────────────────────────────────────────────────────────
  observeEvent(input$btn_search, {
    show_search_modal(input, output, session, shared, search_raw_data)
  })

  # Handle edit actions fired from the search modal (tag / move / delete)
  observeEvent(input$search_action, {
    handle_search_action(input, shared)
  }, ignoreInit = TRUE)

  observeEvent(input$vencidos_action, {
    handle_vencidos_action(input, shared)
  }, ignoreInit = TRUE)

  observeEvent(input$search_stage_toast, {
    payload <- input$search_stage_toast
    if (!is.null(payload) && nzchar(payload$msg %||% "")) {
      showNotification(payload$msg,
                       type     = payload$type %||% "message",
                       duration = 3)
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # ── Ver datos raw data popup ─────────────────────────────────────────────────
  observeEvent(input$btn_ver_datos, {
    raw <- search_raw_data()
    if (is.null(raw) || !nrow(raw)) {
      showNotification("No hay datos cargados.", type = "warning")
      return()
    }
    # Show all raw SAP columns — prioritise Ledger, Empresa first for readability
    disp <- as.data.frame(raw)
    priority_cols <- intersect(c("Ledger","Empresa","Parte","Documento","Moneda"), names(disp))
    rest_cols     <- setdiff(names(disp), priority_cols)
    disp          <- disp[, c(priority_cols, rest_cols), drop = FALSE]
    showModal(modalDialog(
      title     = paste0("\U0001f4cb Datos SAP crudos — ", nrow(disp), " filas"),
      size      = "xl",
      easyClose = TRUE,
      footer    = tagList(
        tags$small(class = "text-muted me-auto",
                   paste0(nrow(disp), " filas \u00d7 ", ncol(disp), " columnas")),
        modalButton("Cerrar")
      ),
      div(style = "max-height:65vh; overflow:auto;",
        DT::datatable(
          disp,
          rownames   = FALSE,
          filter     = "top",
          extensions = "Buttons",
          options    = list(
            pageLength = 25,
            scrollX    = TRUE,
            dom        = "Bfrtip",
            buttons    = list(
              list(extend = "csv",   text = "\U2B07 CSV"),
              list(extend = "excel", text = "\U2B07 Excel")
            )
          )
        )
      )
    ))
  }, ignoreInit = TRUE)

  # ── Control UIs ──────────────────────────────────────────────────────────────
  output$empresa_ui <- renderUI({
    t0_emp   <- proc.time()
    sel      <- empresa_sel_rv()
    snap_inf <- sap_snapshot_info()

    # Returns a formatted datetime string when data is from a snapshot,
    # NULL when data is live (sap_snapshot_info cleared on successful fetch).
    company_snap_lbl <- function(initials) {
      ts_ar <- snap_inf$AR
      ts_ap <- snap_inf$AP
      dates <- Filter(Negate(is.null), list(AR = ts_ar, AP = ts_ap))
      if (!length(dates)) return(NULL)
      latest <- max(unlist(lapply(dates, as.numeric)))
      format(as.POSIXct(latest, origin = "1970-01-01", tz = "UTC"), "%d/%m/%Y %H:%M",
             tz = (input$client_tz %||% "America/Mexico_City"))
    }

    cmap <- company_map_rv()

    out <- div(
      class = "d-flex flex-wrap gap-1 align-items-center",
      lapply(names(cmap), function(initials) {
        nombre   <- cmap[[initials]]
        active   <- nombre %in% sel
        snap_dt  <- company_snap_lbl(initials)
        is_snap  <- !is.null(snap_dt)
        snap_cls <- if (active && is_snap) " snapshot" else ""

        btn <- tags$button(
          id      = paste0("emp_btn_", initials),
          class   = paste0("btn btn-sm emp-toggle-btn",
                           if (active) " active" else "",
                           snap_cls),
          type    = "button",
          onclick = paste0("Shiny.setInputValue('emp_btn_clicked', '", initials,
                           "', {priority:'event'})"),
          initials
        )

        if (active && is_snap) {
          div(class = "emp-tooltip-wrap",
            btn,
            tags$div(class = "emp-tooltip",
              tags$div(style = "font-weight:700; margin-bottom:2px;", "Snapshot"),
              tags$div(paste0("\u00daltima actualizaci\u00f3n: ", snap_dt))
            )
          )
        } else {
          btn
        }
      })
    )
    message(sprintf("[EMPRESA_UI] rendered in %.2fs", (proc.time() - t0_emp)[["elapsed"]]))
    out
  })

  output$month_ui <- renderUI({
    shinyWidgets::airMonthpickerInput("month_sel", NULL,
                                      value = Sys.Date(), autoClose = TRUE)
  })

  output$currency_ui <- renderUI({
    t0_cur_ui  <- proc.time()
    tab        <- input$nav %||% "AR"
    ledger_tab <- if (tab %in% c("AR", "AP")) tab else "AR"
    result <- if (ledger_tab == "AR") {
      choices <- currency_choices$AR()
      if (!length(choices)) choices <- c("MXN", "USD")
      current <- isolate(input$cur_ar)
      if (is.null(current) || !current %in% choices) current <- choices[1]
      selectInput("cur_ar", NULL, choices = choices, selected = current)
    } else {
      choices <- currency_choices$AP()
      if (!length(choices)) choices <- c("MXN", "USD")
      current <- isolate(input$cur_ap)
      if (is.null(current) || !current %in% choices) current <- choices[1]
      selectInput("cur_ap", NULL, choices = choices, selected = current)
    }
    message(sprintf("[CURRENCY_UI] rendered tab=%s in %.2fs",
                    ledger_tab, (proc.time() - t0_cur_ui)[["elapsed"]]))
    result
  })

  # ── Modules ──────────────────────────────────────────────────────────────────
  t0_mod <- proc.time()
  ledgerModuleServer("ar", config = AR_CONFIG, shared = shared)
  message(sprintf("[SERVER] ledger AR registered in %.2fs (session #%d)",
                  (proc.time() - t0_mod)[["elapsed"]], .sn))
  t0_mod <- proc.time()
  ledgerModuleServer("ap", config = AP_CONFIG, shared = shared)
  message(sprintf("[SERVER] ledger AP registered in %.2fs (session #%d)",
                  (proc.time() - t0_mod)[["elapsed"]], .sn))
  t0_mod <- proc.time()
  pagarHoyServer("ph", shared = shared)
  message(sprintf("[SERVER] pagarHoy registered in %.2fs (session #%d)",
                  (proc.time() - t0_mod)[["elapsed"]], .sn))
  t0_mod <- proc.time()
  vencidosServer("ven", shared = shared)
  message(sprintf("[SERVER] vencidos registered in %.2fs (session #%d)",
                  (proc.time() - t0_mod)[["elapsed"]], .sn))
  t0_mod <- proc.time()
  bancosServer("bnc", shared = shared)
  intercoServer("ic", shared = shared)
  reporteServer("rpt", shared = shared, active_tab = reactive(input$nav))
  observeEvent(input$btn_export_rpt, {
    shiny::updateNavbarPage(session, "nav", selected = "RPT")
  }, ignoreInit = TRUE)
  message(sprintf("[SERVER] bancos registered in %.2fs (session #%d)",
                  (proc.time() - t0_mod)[["elapsed"]], .sn))

  # ── Manual entry ─────────────────────────────────────────────────────────────
  observeEvent(input$btn_add_entry, {
    showModal(modalDialog(
      title = "¿Qué tipo de entrada?", size = "s", easyClose = TRUE,
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("pick_ar",    "Cobro \u2014 CxC",  class = "btn btn-primary"),
        actionButton("pick_ap",    "Pago \u2014 CxP",   class = "btn btn-success"),
        actionButton("pick_abono", "Abono parcial",      class = "btn btn-warning")
      )
    ))
  })

  observeEvent(input$pick_ar, {
    active_entry_ledger("AR"); removeModal()
    show_combined_entry_modal("AR", sap_data, session,
                              empresa_vals = empresa_sel_rv())
  }, ignoreInit = TRUE)

  observeEvent(input$pick_ap, {
    active_entry_ledger("AP"); removeModal()
    show_combined_entry_modal("AP", sap_data, session,
                              empresa_vals = empresa_sel_rv())
  }, ignoreInit = TRUE)

  observeEvent(input$pick_abono, {
    removeModal()
    show_abono_modal(sap_data, session)
  }, ignoreInit = TRUE)

  observeEvent(input$me_save, {
    ledger  <- active_entry_ledger()
    req(ledger)
    edit_id <- session$userData[["me_edit_id"]] %||% ""
    doc     <- trimws(input$me_documento %||% "")
    if (!nzchar(doc)) {
      showNotification("El campo Documento es obligatorio.", type = "warning")
      return()
    }

    if (nzchar(edit_id)) {
      # ── Edit mode: replace existing row by id ────────────────────────────
      existing <- manual_inv() |> dplyr::filter(.data$id == edit_id)
      new_row <- tibble::tibble(
        id                     = edit_id,
        ledger                 = ledger,
        Empresa                = input$me_empresa,
        Moneda                 = toupper(trimws(input$me_moneda)),
        Documento              = doc,
        Factura                = trimws(input$me_factura  %||% ""),
        Parte                  = trimws(input$me_parte    %||% ""),
        Codigo                 = trimws(input$me_codigo   %||% ""),
        Importe                = input$me_importe         %||% 0,
        `Fecha de vencimiento` = as.Date(input$me_fecha),
        Notas                  = trimws(input$me_notas    %||% ""),
        created_by             = if (nrow(existing)) existing$created_by[1] else current_user(),
        created_at             = if (nrow(existing)) existing$created_at[1] else Sys.time(),
        updated_at             = Sys.time()
      )
      df <- upsert_manual(manual_inv(), new_row)
      manual_inv(df)
      tryCatch(save_manual(df),
               error = function(e) showNotification(
                 "No se pudieron guardar los cambios.", type = "warning"))
      .sync_staged(
        data.frame(Empresa   = input$me_empresa,
                   Moneda    = toupper(trimws(input$me_moneda)),
                   Documento = doc,
                   stringsAsFactors = FALSE),
        ledger     = ledger,
        ph_rv      = pagar_hoy_db,
        new_date   = as.Date(input$me_fecha),
        new_imp    = input$me_importe  %||% 0,
        new_parte  = trimws(input$me_parte %||% ""))
      session$userData[["me_edit_id"]] <- ""
      removeModal()
      active_entry_ledger(NULL)
      showNotification("Entrada actualizada.", type = "message", duration = 2)

    } else {
      # ── Insert mode ──────────────────────────────────────────────────────
      new_row <- tibble::tibble(
        id                     = uuid::UUIDgenerate(),
        ledger                 = ledger,
        Empresa                = input$me_empresa,
        Moneda                 = toupper(trimws(input$me_moneda)),
        Documento              = doc,
        Factura                = trimws(input$me_factura  %||% ""),
        Parte                  = trimws(input$me_parte    %||% ""),
        Codigo                 = trimws(input$me_codigo   %||% ""),
        Importe                = input$me_importe         %||% 0,
        `Fecha de vencimiento` = as.Date(input$me_fecha),
        Notas                  = trimws(input$me_notas    %||% ""),
        created_by             = current_user(),
        created_at             = Sys.time(),
        updated_at             = Sys.time()
      )
      df <- upsert_manual(manual_inv(), new_row)
      manual_inv(df)
      tryCatch(save_manual(df),
               error = function(e) showNotification(
                 "No se pudieron guardar los cambios.", type = "warning"))

      # ── Stage to Agenda de hoy if toggle is active ───────────────────────
      send_to_agenda <- isTRUE(input$me_agenda_active)
      if (send_to_agenda) {
        ph_row <- tibble::tibble(
          id        = new_row$id,
          ledger    = ledger,
          Empresa   = input$me_empresa,
          Moneda    = toupper(trimws(input$me_moneda)),
          Documento = doc,
          Parte     = trimws(input$me_parte    %||% ""),
          Codigo    = trimws(input$me_codigo   %||% ""),
          tipo_item = "factura",
          Importe   = input$me_importe         %||% 0,
          FechaVenc = as.Date(input$me_fecha),
          staged_by = current_user(),
          staged_at = Sys.time(),
          status    = "pending"
        )
        ph_updated <- upsert_pagar_hoy(
          pagar_hoy_db() %||% safe_load_pagar_hoy(current_user()),
          ph_row, keys = "id")
        pagar_hoy_db(ph_updated)
        tryCatch(save_pagar_hoy(ph_updated, current_user()),
                 error = function(e) showNotification(
                   paste("No se pudo guardar en Agenda:", e$message), type = "warning"))
      }

      removeModal()
      active_entry_ledger(NULL)
      showNotification(
        if (send_to_agenda) "Entrada agregada y enviada a Agenda de hoy."
        else "Entrada agregada.",
        type = "message", duration = 2)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$me_cancel, {
    session$userData[["me_edit_id"]] <- ""
    removeModal(); active_entry_ledger(NULL)
  }, ignoreInit = TRUE)

  # ── Supplier suggestions in Nueva entrada – AP modal ──────────────────────────
  me_prov_query   <- reactive({ input$me_parte %||% "" })
  me_prov_query_d <- debounce(me_prov_query, 300)

  output$me_prov_suggestions <- renderUI({
    req(isTRUE(active_entry_ledger() == "AP"))
    q <- trimws(me_prov_query_d())
    if (nchar(q) < 2) return(NULL)

    provs <- shared$proveedores_db() %||% data.frame()
    if (!nrow(provs)) return(NULL)

    matches <- tryCatch(
      find_proveedor_matches(
        query          = list(parte = q, rfc = "", no_cuenta = "", nombre = q, alias = ""),
        proveedores_df = provs,
        threshold      = 15L,
        top_n          = 8L
      ),
      error = function(e) NULL
    )
    if (is.null(matches) || !nrow(matches)) return(NULL)

    # Build IC detection sets for the currently selected empresa (AP context)
    empresa_full <- input$me_empresa %||% ""
    empresa_init <- names(company_map_rv())[company_map_rv() == empresa_full][1] %||% NA_character_
    registry     <- shared$interco_v2()
    ap_ic_codes  <- if (!is.null(registry) && !is.na(empresa_init) &&
                        !is.null(registry$companies[[empresa_init]])) {
      prefix <- registry$ap_prefix %||% "P"
      toupper(paste0(prefix, registry$companies[[empresa_init]]$ap %||% character()))
    } else character()
    # IC RFCs: use the GLOBAL set of all known IC company RFCs (not filtered by
    # empresa).  The rfcs dict accumulates RFCs from every scanner run, so even
    # when NL/NCS/NRS have no open IC invoices today, the RFCs of their IC partners
    # (scanned from NTS/NG data) are still available for badge detection.
    ap_ic_rfcs <- if (!is.null(registry$rfcs) && length(registry$rfcs)) {
      unique(toupper(trimws(unname(registry$rfcs))))
    } else character()
    ap_ic_rfcs <- ap_ic_rfcs[nzchar(ap_ic_rfcs)]

    div(class = "prov-suggest-box",
      style = "border:1px solid #dee2e6; border-radius:6px; margin-top:-8px; margin-bottom:8px; overflow:hidden;",
      lapply(seq_len(nrow(matches)), function(i) {
        m         <- matches[i, ]
        score_pct <- paste0(min(m$.score, 100L), "%")
        payload   <- jsonlite::toJSON(
          list(nombre = m$nombre %||% "", alias = m$alias %||% "", moneda = m$moneda %||% ""),
          auto_unbox = TRUE
        )
        # Match by CardCode OR by RFC (catálogo codigo field is often unpopulated)
        is_ic_sug <- length(ap_ic_codes) > 0 && (
          (nzchar(m$codigo %||% "") && toupper(m$codigo %||% "") %in% ap_ic_codes) ||
          (nzchar(m$rfc    %||% "") && toupper(m$rfc    %||% "") %in% ap_ic_rfcs)
        )

        tags$div(
          class       = "prov-suggest-item d-flex align-items-center gap-2 px-3 py-2",
          style       = if (is_ic_sug)
            "cursor:pointer; border-bottom:1px solid #f0f0f0; background:#f0f4ff; transition:background 0.1s;"
          else
            "cursor:pointer; border-bottom:1px solid #f0f0f0; transition:background 0.1s;",
          onmouseover = "this.style.background='#e8eef8'",
          onmouseout  = if (is_ic_sug) "this.style.background='#f0f4ff'"
                        else           "this.style.background=''",
          onclick     = paste0(
            "Shiny.setInputValue('me_prov_pick', ", payload, ", {priority:'event'})"
          ),
          tags$span(class = "fw-semibold flex-grow-1 small", m$nombre %||% ""),
          if (is_ic_sug) tags$span(
            class = "badge",
            style = "background:#0d6efd; color:#fff; font-size:0.65em; white-space:nowrap;",
            "Intercompany"
          ) else NULL,
          tags$span(class = "text-muted small", m$alias %||% ""),
          tags$span(
            class = "badge ms-auto",
            style = "background:#e9ecef; color:#495057; font-size:0.7em;",
            score_pct
          )
        )
      })
    )
  })

  observeEvent(input$me_prov_pick, {
    p <- input$me_prov_pick
    req(p)
    updateTextInput(session, "me_parte",  value = p$nombre %||% "")
    updateTextInput(session, "me_codigo", value = p$alias  %||% "")
    if (!is.null(p$moneda) && nzchar(p$moneda %||% ""))
      updateSelectInput(session, "me_moneda", selected = p$moneda)
  }, ignoreInit = TRUE)

  # ── Notes ─────────────────────────────────────────────────────────────────────
  notes_handlers(input, output, session, shared, notes_df, current_user)

  # ── Settings ──────────────────────────────────────────────────────────────────
  observeEvent(input$btn_settings, {
    show_settings_modal(input, output, session, shared)
  }, ignoreInit = TRUE)
  settings_observers(input, output, session, shared)
  settings_catalogo_edit_observer(input, output, session, shared)
  settings_alias_counter(input, output, session)
  settings_cancel_edit_observer(input, output, session)
  settings_cuentas_observer(input, output, session, shared)
  settings_usuarios_observer(input, output, session, shared)
  settings_sincro_observer(input, output, session, shared, pagar_hoy_db)
  settings_policies_observer(input, output, session, shared)
  settings_companies_observer(input, output, session, shared)

  # ── Grupo modules (Usuarios + Empresas) ───────────────────────────────────────
  tiersServer("trs",    shared = shared)
  empresasServer("emp", shared = shared)

  message(sprintf("[SERVER] registration complete at t+%.2fs (session #%d) — reactive flush starting",
                  (proc.time() - .t_session)[["elapsed"]], .sn))
}

shinyApp(ui, server)