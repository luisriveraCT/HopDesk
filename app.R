# =============================================================================
# app.R
# =============================================================================

# Bootstrap: source global.R if it hasn't been loaded yet in this R session.
# Covers the "Run App" button workflow where the user hasn't manually sourced
# global.R first. The guard avoids double-sourcing in the normal dev workflow
# (source("R/global.R") then runApp()).
if (!exists("S3_KEYS", envir = .GlobalEnv)) source("R/global.R")

source("R/pagar_hoy_module.R")
source("R/search_module.R")
source("R/vencidos_module.R")
source("R/treasury_map_module.R")
source("R/reporte_module.R")
source("R/audit_log_viewer_module.R")
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
source("R/cashflow_preview_module.R")
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
  selected = "CAL",
  header   = tagList(
    shinyjs::useShinyjs(),
    app_styles(),
    app_scripts(),
    tags$link(rel = "stylesheet", type = "text/css", href = "cashflow_preview.css?v=6"),
    # Hide shinymanager's default floating session/logout widget (uses .mfb-* classes)
    tags$style(HTML("[id*=shinymanager],[class*=shinymanager],[class*=mfb-]{display:none!important}")),
    # Hide GRUPO menu by default — server shows it only for dev / admin tier
    tags$style(HTML(".navbar .nav-link[data-value='GRUPO']{display:none!important}")),
    # In admin deployment: hide all financial tabs until a client context is jumped into
    if (IS_ADMIN_DEPLOYMENT) tags$style(HTML(paste0(
      ".navbar .nav-link[data-value='CAL'],",
      ".navbar .nav-link[data-value='VEN'],",
      ".navbar .nav-link[data-value='PH'],",
      ".navbar .nav-link[data-value='BNC'],",
      ".navbar .nav-link[data-value='IC'],",
      ".navbar .nav-link[data-value='PSV'],",
      ".navbar .nav-link[data-value='FC'],",
      ".navbar .nav-link[data-value='RPT']",
      "{display:none!important}"
    ))),
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
          var noBar   = val === 'RPT' || val === 'TIERS' || val === 'EMP';
          if (bar)     bar.style.display    = noBar ? 'none' : '';
          if (bar)     bar.classList.toggle('cb-mode-cal', val === 'CAL');
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
      class = "control-bar bg-white",
      div(
        class = "cb-row",
        # Left zone — company pills (scrollable)
        div(class = "cb-zone-emp",
          div(class = "emp-strip-outer",
            div(class = "emp-strip", id = "emp-strip",
              uiOutput("empresa_ui")
            )
          )
        ),
        # Center zone — controls grouped in a card
        div(class = "cb-zone-controls",
          div(class = "cb-field cb-month",
            uiOutput("month_ui")
          ),
          div(class = "cb-field cb-currency",
            uiOutput("currency_ui")
          ),
          shinyWidgets::radioGroupButtons(
            inputId  = "ic_mode", label = NULL,
            choices  = c("Sin IC" = "exclude", "Con IC" = "include", "Sólo IC" = "only"),
            selected = "exclude", size = "sm", status = "outline-secondary"
          )
        ),
        # Right zone — action buttons
        div(class = "cb-zone-actions d-flex align-items-center gap-2",
          actionButton("btn_export",    icon("file-export"),       label = NULL,
                       class = "btn btn-outline-secondary btn-sm cb-act",
                       title = "Exportar Flujo de Caja"),
          actionButton("btn_refresh",   icon("rotate"),           label = NULL,
                       class = "btn btn-outline-secondary btn-sm cb-act",
                       title = "Actualizar datos de ERP"),
          actionButton("btn_search",    icon("magnifying-glass"), label = NULL,
                       class = "btn btn-outline-secondary btn-sm cb-act",
                       title = "Buscar facturas del mes"),
          actionButton("btn_settings",  icon("gear"),             label = NULL,
                       class = "btn btn-outline-secondary btn-sm cb-act",
                       title = "Configuración"),
          actionButton("btn_add_entry", icon("plus"), label = " Agregar",
                       class = "btn btn-primary btn-sm")
        )
      )
    ),
    # Notes bars + panels live here (top-level DOM) so position:fixed isn't
    # clipped by bslib's tab-content overflow container.
    div(id = "notes_bar", class = "notes-toggle-bar",
      actionButton("toggle_notes",
                   tagList(icon("sticky-note"), " Notas del mes"),
                   class = "notes-bar-btn")
    ),
    uiOutput("notes_panel")
  ),

  bslib::nav_panel(
    title = tagList(icon("calendar"), " Calendario"), value = "CAL",
    # cf-slide-wrapper clips overflow so only the visible panel shows.
    # .cf-preview-active (toggled by cashflowPreviewServer) slides both
    # panels left by 100%, revealing the preview and hiding the calendar.
    div(class = "h-100 cf-slide-wrapper",
      # Panel A — Calendar
      div(id = "cf-panel-calendar", class = "cf-panel cf-panel-calendar h-100",
        div(id = "cal_ar_wrapper", class = "h-100 position-relative",
          ledgerModuleUI("ar")
        ),
        shinyjs::hidden(
          div(id = "cal_ap_wrapper", class = "h-100 position-relative",
            ledgerModuleUI("ap")
          )
        )
      ),
      # Panel B — Cash Flow Preview (Stage 1: placeholder; content added in later stages)
      cashflowPreviewUI("cfp")
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
    title = tagList(icon("chart-line"), " Forecasting"),
    value = "FC",
    div(class = "h-100 overflow-auto",
      forecastingUI("fc")
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

    # ── Loading overlay (www/overlay_init.js) ────────────────────────────────
    tags$script(src = "overlay_init.js?v=6"),

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

# ── IMPORTANT: Shiny global reactive environment is a process-level singleton ──
# shiny:::.getReactiveEnvironment() is ONE object shared across ALL runApp()
# calls in the same R session.  Its internal priority queue (.priorities vector
# + .itemsByPriority fastmap) can be left corrupted when an app run crashes
# mid-flush: .priorities still contains entries whose map keys were already
# consumed, so every subsequent runApp() in THAT session immediately fails with:
#   "Error in ctx$executeFlushCallbacks() : attempt to apply non-function"
# (dequeue returns NULL because the map has no item for the stale priority).
#
# The fix is ALWAYS to restart R before re-running after a crash.
# Do NOT try to debug by re-sourcing in the same session — the patches
# themselves corrupt the queue further (environment(fn)<-foreign_env severs
# the closure, making orig_fn unfindable and producing silent infinite
# recursion).  If you must diagnose in-session, reset the queue first:
#
#   re  <- shiny:::.getReactiveEnvironment()
#   pf  <- get(".pendingFlush", envir = environment(get("flush", envir = re)))
#   env <- environment(get("dequeue", envir = pf))
#   assign(".priorities",      numeric(0),            envir = env)
#   assign(".itemsByPriority", shiny:::Map$new(),      envir = env)
#
# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Session diagnostics ──────────────────────────────────────────────────────
  .t_session <- proc.time()
  .GlobalEnv$.session_count <- .GlobalEnv$.session_count + 1L
  .sn <- .GlobalEnv$.session_count
  message(sprintf("[SESSION] server() #%d started at %s  (1=auth gate, 2=real app)",
                  .sn, format(Sys.time(), "%H:%M:%S")))

  # Tell the browser to show (or keep showing) the loading overlay the instant
  # session #2 connects.  Priority 10 fires before Phase 1 (priority 0) so the
  # message is queued in the very first flush — covering the shinymanager
  # session-swap gap where the overlay could have been lost.
  if (.sn >= 2L) {
    observe({
      session$sendCustomMessage("showOverlay", list())
    }, priority = 10)
  }

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
    user_val  <- tryCatch(res_auth$user,            error = function(e) NULL)
    tier_val  <- tryCatch(res_auth$tier,            error = function(e) NULL)
    name_val  <- tryCatch(res_auth$name,            error = function(e) NULL)
    code_val  <- tryCatch(res_auth$account_code,    error = function(e) NULL)
    gids_val  <- tryCatch(res_auth$group_ids,       error = function(e) NULL)
    cid_val   <- tryCatch(res_auth$client_id,       error = function(e) NULL)
    ac_val    <- tryCatch(res_auth$allowed_clients, error = function(e) NULL)
    .cid <- function(v) if (!is.null(v) && nzchar(v %||% "")) v else tolower(Sys.getenv("CLIENT_ID"))
    if (is.null(user_val) || !nzchar(user_val %||% ""))
      return(list(user = "unknown", name = "unknown", tier = "finance",
                  account_code = "", group_ids = "[]",
                  client_id = tolower(Sys.getenv("CLIENT_ID")),
                  allowed_clients = "[]", is_staff = FALSE))
    list(
      user            = user_val %||% "unknown",
      name            = name_val %||% user_val %||% "unknown",
      tier            = tier_val %||% "finance",
      account_code    = code_val %||% "",
      group_ids       = gids_val %||% "[]",
      client_id       = .cid(cid_val),
      allowed_clients = ac_val   %||% "[]",
      is_staff        = is_staff_tier(tier_val %||% "finance")
    )
  })

  # ── Emergency lock: terminate active sessions of locked accounts ─────────────
  # Runs once on session init. If the just-authenticated user appears in the
  # emergency lock file, reload the session immediately — this handles the case
  # where an account is locked while already logged in.
  #
  # Manual guard flag instead of observeEvent(..., once = TRUE) — same fix as
  # redirected_to_staff_home() below: once + req() self-destroys on the very
  # first firing of current_user_info() (its "unknown" placeholder, before
  # auth resolves) regardless of whether req() passed, so the check never
  # actually ran for the real user. This silently disabled the emergency-lock
  # kick-out for already-active sessions.
  emergency_lock_checked <- reactiveVal(FALSE)
  observe({
    info <- current_user_info()
    if (isTRUE(emergency_lock_checked())) return()
    if (is.null(info) || info$user == "unknown") return()
    emergency_lock_checked(TRUE)
    lock <- tryCatch(read_emergency_lock(), error = function(e) NULL)
    if (is.data.frame(lock) && nrow(lock) > 0 &&
        any(tolower(lock$username) == tolower(info$user))) {
      message("[SECURITY] Active session terminated — account locked: ", info$user)
      session$reload()
    }
  })

  # ── First-login password change gate ─────────────────────────────────────────
  # Accounts created via direct script (not the invite flow) have
  # requires_password_change = TRUE. If set, intercept navigation here and show
  # only the password-change UI — no modules are accessible until the password is set.
  #
  # Same once + req() fix as above.
  force_pw_checked <- reactiveVal(FALSE)
  observe({
    info <- current_user_info()
    if (isTRUE(force_pw_checked())) return()
    if (is.null(info) || info$user == "unknown") return()
    force_pw_checked(TRUE)
    usuarios <- tryCatch(auth_load_usuarios(), error = function(e) NULL)
    if (is.null(usuarios) || !nrow(usuarios)) return()
    row <- usuarios[tolower(usuarios$username) == tolower(info$user), , drop = FALSE]
    if (!nrow(row)) return()
    needs_change <- isTRUE(row$requires_password_change[1])
    if (needs_change) {
      showModal(modalDialog(
        title   = tagList(icon("lock"), " Cambio de contraseña requerido"),
        footer  = NULL,
        easyClose = FALSE,
        size    = "s",
        tags$p(class = "text-muted small",
               "Tu contraseña fue asignada por el administrador. Debes establecer una nueva antes de continuar."),
        passwordInput("force_pw_new",  "Nueva contraseña", width = "100%"),
        passwordInput("force_pw_conf", "Confirmar contraseña", width = "100%"),
        uiOutput("force_pw_error"),
        actionButton("force_pw_submit", "Confirmar contraseña",
                     class = "btn btn-primary w-100 mt-2")
      ))
    }
  })

  # ── Stage 4 / Stage 2 Part A: set home_client_id from user record at login ────
  # Sets once per session and never changes again for the session's lifetime —
  # this is the user's home, not their current context. Staff (client_id =
  # "hd-admin") have an empty home; client users are locked to their own
  # prefix and have no context switcher.
  #
  # Deliberately NOT once = TRUE: current_user_info() legitimately fires once
  # with its "unknown" placeholder before shinymanager's auth resolves, and
  # observeEvent(once = TRUE) self-destroys on that very first firing
  # regardless of whether the inner req() below passed — req() only stops
  # that one invocation, it does not stop the once-wrapper from tearing the
  # whole observer down. That combination left home_client_id() permanently
  # NULL for every session, silently falling back through effective_client_id()
  # %||% to Sys.getenv("CLIENT_ID") (always "hd-admin" in this shared
  # deployment) — a native client user ended up reading Hopdesk's own S3
  # folder instead of their own. Idempotency is enforced explicitly instead,
  # by checking home_client_id() is still unset.
  observeEvent(current_user_info(), {
    if (!is.null(home_client_id())) return()
    info <- current_user_info()
    req(!is.null(info$user) && nzchar(info$user) && info$user != "unknown")
    cid <- info$client_id
    if (!is.null(cid) && nzchar(cid)) home_client_id(cid)
  }, ignoreNULL = TRUE)

  output$force_pw_error <- renderUI(NULL)

  observeEvent(input$force_pw_submit, {
    pw1 <- input$force_pw_new
    pw2 <- input$force_pw_conf
    if (!nzchar(pw1) || nchar(pw1) < 6 ||
        !grepl("[0-9]", pw1) || !grepl("[^A-Za-z0-9]", pw1)) {
      output$force_pw_error <- renderUI(
        tags$p(class = "text-danger small mt-1",
               "La contraseña debe tener al menos 6 caracteres, un número y un símbolo.")
      )
      return()
    }
    if (pw1 != pw2) {
      output$force_pw_error <- renderUI(
        tags$p(class = "text-danger small mt-1", "Las contraseñas no coinciden.")
      )
      return()
    }
    info     <- current_user_info()
    usuarios <- tryCatch(auth_load_usuarios(), error = function(e) NULL)
    if (is.null(usuarios)) return()
    idx <- which(tolower(usuarios$username) == tolower(info$user))
    if (!length(idx)) return()
    usuarios$password_hash[idx]            <- pw1
    usuarios$requires_password_change[idx] <- FALSE
    tryCatch({
      auth_save_usuarios(usuarios)
      removeModal()
      showNotification("Contraseña actualizada. Bienvenido.", type = "message", duration = 4)
    }, error = function(e) {
      output$force_pw_error <- renderUI(
        tags$p(class = "text-danger small mt-1", paste("Error al guardar:", e$message))
      )
    })
  }, ignoreInit = TRUE)

  # ── Invite token interceptor ─────────────────────────────────────────────────
  # If the URL contains ?invite=<token>, intercept before the user reaches any
  # module, resolve the token, and show a one-time account-setup modal.
  # The token is single-use: marked "accepted" after the account is created.
  invite_token_rv <- reactive({
    qs <- session$clientData$url_search
    if (!nzchar(qs %||% "")) return(NULL)
    params <- tryCatch({
      pairs <- strsplit(sub("^\\?", "", qs), "&")[[1]]
      kv <- strsplit(pairs, "=")
      setNames(lapply(kv, `[[`, 2), sapply(kv, `[[`, 1))
    }, error = function(e) list())
    params[["invite"]] %||% NULL
  })

  observeEvent(invite_token_rv(), {
    token <- invite_token_rv()
    req(nzchar(token %||% ""))
    invite <- tryCatch(resolve_invite(token), error = function(e) NULL)
    if (is.null(invite)) {
      showModal(modalDialog(
        title = "Enlace no válido",
        tags$p("Este enlace de invitación ha expirado o ya fue utilizado."),
        footer = modalButton("Cerrar"), easyClose = TRUE
      ))
      return()
    }
    showModal(modalDialog(
      title = tagList(icon("user-plus"), " Activar cuenta"),
      tags$p("Bienvenido a HopDesk. Crea tu contraseña para activar la cuenta ",
             tags$strong(invite$email), "."),
      passwordInput("invite_pw_new",  "Nueva contraseña"),
      passwordInput("invite_pw_conf", "Confirmar contraseña"),
      uiOutput("invite_pw_error"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("invite_pw_submit", "Activar cuenta",
                     class = "btn btn-primary")
      ),
      easyClose = FALSE
    ))
  }, ignoreNULL = TRUE, once = TRUE)

  output$invite_pw_error <- renderUI(NULL)

  observeEvent(input$invite_pw_submit, {
    token <- invite_token_rv()
    req(nzchar(token %||% ""))
    pw1 <- input$invite_pw_new
    pw2 <- input$invite_pw_conf
    if (!nzchar(pw1) || nchar(pw1) < 6 ||
        !grepl("[0-9]", pw1) || !grepl("[^A-Za-z0-9]", pw1)) {
      output$invite_pw_error <- renderUI(
        tags$p(class = "text-danger small mt-1",
               "La contraseña debe tener al menos 6 caracteres, un número y un símbolo.")
      )
      return()
    }
    if (pw1 != pw2) {
      output$invite_pw_error <- renderUI(
        tags$p(class = "text-danger small mt-1", "Las contraseñas no coinciden.")
      )
      return()
    }
    invite <- tryCatch(resolve_invite(token), error = function(e) NULL)
    if (is.null(invite)) {
      output$invite_pw_error <- renderUI(
        tags$p(class = "text-danger small mt-1",
               "El enlace ya no es válido. Solicita una nueva invitación.")
      )
      return()
    }
    # Create the user account in the target client's folder
    tryCatch({
      usuarios <- auth_load_usuarios(client_id = invite$client_id)
      username <- tolower(gsub("[^a-z0-9]", "", invite$display_name))
      # Ensure unique username
      if (username %in% tolower(usuarios$username)) {
        username <- paste0(username, substr(token, 1, 4))
      }
      new_user <- data.frame(
        id                       = paste0("u_", format(Sys.time(), "%Y%m%d%H%M%S")),
        account_code             = sprintf("U%04d", nrow(usuarios) + 1L),
        username                 = username,
        password_hash            = pw1,
        display_name             = invite$display_name,
        tier                     = invite$tier,
        client_id                = invite$client_id,
        permisos                 = "{}",
        group_ids                = "[]",
        allowed_clients          = "[]",
        email                    = invite$email,
        requires_password_change = FALSE,
        activo                   = TRUE,
        created_at               = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        last_login               = NA_character_,
        deleted                  = FALSE,
        deleted_at               = NA_character_,
        stringsAsFactors = FALSE
      )
      auth_save_usuarios(rbind(usuarios, new_user), client_id = invite$client_id)
      accept_invite(token)
      register_username(username, invite$client_id, new_user$account_code)
      update_client_user_count(invite$client_id)
      tryCatch(notify_user_limit(invite$client_id),
               error = function(e) message("[INVITE] notify_user_limit failed: ", e$message))
      removeModal()
      showNotification(
        paste0("Cuenta activada. Inicia sesión como '", username, "'."),
        type = "message", duration = 6
      )
    }, error = function(e) {
      output$invite_pw_error <- renderUI(
        tags$p(class = "text-danger small mt-1", paste("Error:", e$message))
      )
    })
  }, ignoreInit = TRUE)

  # ── Shared state ────────────────────────────────────────────────────────────
  # All auxiliary objects are pre-loaded in global.R at process start.
  # Reading from .app_data_cache is instant (in-memory) vs ~400ms per S3 call.
  cf_preview_open   <- reactiveVal(FALSE)   # TRUE = preview panel visible
  cf_preview_prefill <- reactiveVal(NULL)   # Stage 5: export modal pre-fill payload
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
  proveedores_db     <- reactiveVal(NULL)
  parte_alias_map_db <- reactiveVal(NULL)   # populated in Phase 2 via register_synced
  conciliacion_rv    <- reactiveVal(NULL)   # populated in Phase 2 via register_synced
  proveedores_inactivos_db <- reactiveVal(NULL)
  papelera_rv           <- reactiveVal(NULL)
  audit_mode            <- reactiveVal(FALSE)
  active_entry_ledger   <- reactiveVal(NULL)
  sap_loading       <- reactiveVal(FALSE)
  sap_snapshot_info <- reactiveVal(list(AR = NULL, AP = NULL))
  search_raw_data   <- reactiveVal(NULL)
  current_user      <- reactive({
    # Prefer res_auth-driven current_user_info (reliable across overlay logins)
    ci <- tryCatch(current_user_info(), error = function(e) NULL)
    if (!is.null(ci) && nzchar(ci$user %||% "") && ci$user != "unknown") return(ci$user)
    # Fallback: shinymanager session cookie
    sm <- tryCatch(shinymanager::get_session_info(session), error = function(e) NULL)
    if (!is.null(sm) && nzchar(sm$user %||% "")) sm$user
    else session$user %||% "anon"
  })
  ctas_cuentas      <- reactiveVal(NULL)
  # Bancos module reactive vals
  bancos_movimientos_db <- reactiveVal(NULL)
  bancos_cuentas_db     <- reactiveVal(NULL)
  bancos_confirmados_db <- reactiveVal(NULL)
  .s3_load_complete     <- reactiveVal(FALSE)
  .phase1_done          <- reactiveVal(FALSE)

  # ── Empresa data — live source from empresas.rds ─────────────────────────────
  # Populated by empresasServer after load/save; falls back to static COMPANY_MAP.
  empresas_db <- reactiveVal(NULL)

  # ── Grupos data — conglomerate / client-group registry ───────────────────────
  grupos_db        <- reactiveVal(NULL)

  # Stage 2 Part A: home vs. jump are two structurally separate reactiveVals —
  # never one conflated variable. home_client_id is set once at login and never
  # changes; jump_client_id is NULL unless a staff session is mid-jump (only a
  # session where current_user_info()$is_staff is TRUE may ever set it — see
  # tiers_module.R's context switcher). effective_client_id is what every data
  # loader keys off: the jumped client if any, else home.
  home_client_id      <- reactiveVal(NULL)
  jump_client_id      <- reactiveVal(NULL)
  effective_client_id <- reactive({ jump_client_id() %||% home_client_id() })

  hop_grants_db    <- reactiveVal(.schema_hop_grants())

  # S3-folder isolation is the real fence: CLIENT_ID prefix in .s3_key() ensures
  # every deployment reads only its own folder (e.g. networks/ or hopdesk/).
  # No in-app filtering by visible_initials needed — stub returns NULL (= no filter).
  visible_initials <- reactive({ NULL })

  # Named vector: initials → nombre_corto for active, non-deleted companies.
  # Falls back to the static COMPANY_MAP only while empresas_db is NULL (loading).
  # An empty data.frame means the client has no companies — return c(), never the
  # hardcoded Networks fallback. This prevents cross-client data leakage when
  # switching from a populated client (e.g. networks) to an empty one (e.g. hopdesk).
  company_map_rv <- reactive({
    # Admin deployment: for a STAFF session, company pills only make sense in
    # a jumped client context — hd-admin's own S3 prefix may contain stale or
    # seeded empresa data, so ignore it while staff sit at home. This must
    # NOT apply to a native client session: jump_client_id() is a staff-only
    # concept and is always NULL for a client user, so checking it alone here
    # was zeroing out every native client's own real company list too (found
    # during Stage 3b, 2026-07-16 — the actual gate is is_staff, not "is
    # jump_client_id set").
    if (IS_ADMIN_DEPLOYMENT) {
      is_staff <- isTRUE(tryCatch(current_user_info()$is_staff, error = function(e) FALSE))
      if (is_staff && is.null(tryCatch(jump_client_id(), error = function(e) NULL))) return(c())
    }
    df <- empresas_db()
    if (is.null(df)) return(COMPANY_MAP)  # NULL = still loading; keep static seed
    active <- df[
      (is.na(df$deleted) | df$deleted != TRUE) &
      (isTRUE(df$activa) | df$activa == TRUE), , drop = FALSE]
    if (!nrow(active)) c() else setNames(active$nombre_corto, active$initials)
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
    if (!identical(new_sel, cur_sel)) empresa_sel_rv(new_sel)
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

  # Initialize empty; populated in Phase 2 alongside the other deferred keys.
  # Avoids two synchronous S3 reads blocking the server() body at startup.
  pasivos_provisions_db        <- reactiveVal(.schema_pasivos_provision())
  pasivos_liabilities_db       <- reactiveVal(.schema_pasivos_liability())
  # One-shot flag: set TRUE by PPM save handlers before they update
  # pasivos_provisions_db so the ledger auto-refresh observer skips re-opening
  # the calendar modal after an external provision add.
  suppress_ledger_prov_refresh <- reactiveVal(FALSE)

  # Forecasting reactiveVals — populated in Phase 2.
  forecasting_series_observations_db <- reactiveVal(.schema_forecasting_series_observations())
  forecasting_metrics_db             <- reactiveVal(.schema_forecasting_metrics())
  forecasting_subscriptions_db       <- reactiveVal(.schema_forecasting_subscriptions())
  forecasting_manual_curves_db       <- reactiveVal(.schema_forecasting_manual_curves())
  active_cal_ledger                  <- reactiveVal("AR")
  # .cal_fade_pending     <- reactiveVal(FALSE)  # OVERLAY FADE — abandoned (see observer below)
  # .ar_post_phase2_count <- reactiveVal(0L)     # OVERLAY FADE — abandoned (see observer below)
  group_config_rv <- reactiveVal(
    tryCatch(load_group_config(),
             error = function(e) list(group_name = "", logo_raw = NULL, logo_ext = "png"))
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
    active_cal_ledger   = active_cal_ledger,
    active_entry_ledger = active_entry_ledger,
    current_user        = current_user,
    current_user_info   = current_user_info,
    amount_col        = reactive("Saldo vencido"),
    empresa_sel       = empresa_sel_rv,
    empresas_db       = empresas_db,
    grupos_db         = grupos_db,
    home_client_id      = home_client_id,
    jump_client_id      = jump_client_id,
    effective_client_id = effective_client_id,
    hop_grants_db     = hop_grants_db,
    visible_initials  = visible_initials,
    company_map       = company_map_rv,
    ctas_cuentas           = ctas_cuentas,
    bancos_movimientos     = bancos_movimientos_db,
    bancos_movimientos_db  = bancos_movimientos_db,   # alias: sync_bus uses this name
    bancos_cuentas         = bancos_cuentas_db,
    bancos_confirmados     = bancos_confirmados_db,
    bancos_confirmados_db  = bancos_confirmados_db,   # alias: sync_bus uses this name
    conciliacion_rv        = conciliacion_rv,
    pasivos_provisions_db        = pasivos_provisions_db,
    pasivos_liabilities_db       = pasivos_liabilities_db,
    suppress_ledger_prov_refresh = suppress_ledger_prov_refresh,
    forecasting_series_observations_db = forecasting_series_observations_db,
    forecasting_metrics_db             = forecasting_metrics_db,
    forecasting_subscriptions_db       = forecasting_subscriptions_db,
    forecasting_manual_curves_db       = forecasting_manual_curves_db,
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
    ),
    group_config   = group_config_rv,
    group_name     = reactive({ group_config_rv()$group_name %||% "" }),
    group_logo_raw = reactive({ group_config_rv()$logo_raw }),
    group_logo_ext = reactive({ group_config_rv()$logo_ext %||% "png" }),
    cf_preview_open   = cf_preview_open,
    cf_preview_prefill = cf_preview_prefill
  )

  # ── Cash Flow Preview toggle ─────────────────────────────────────────────────
  observeEvent(input$btn_preview_open, {
    cf_preview_open(!cf_preview_open())
  }, ignoreInit = TRUE)

  # ── Calendar ledger toggle (Cobros / Pagos pill) ────────────────────────────
  observeEvent(input$cal_ledger_toggle, {
    ledger <- input$cal_ledger_toggle
    if (!ledger %in% c("AR", "AP")) return()
    active_cal_ledger(ledger)
    if (ledger == "AR") {
      shinyjs::show("cal_ar_wrapper"); shinyjs::hide("cal_ap_wrapper")
    } else {
      shinyjs::hide("cal_ar_wrapper"); shinyjs::show("cal_ap_wrapper")
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # ── Cross-session sync bus ───────────────────────────────────────────────────
  # Registry-based: every shared reactiveVal declared here propagates to other
  # sessions within 3s of its save_*() call bumping the version.
  # pagar_hoy_db uses the in-memory .agenda_sync$data slot (no S3 hit);
  # all other keys re-read from S3 only when their version changes.
  # All loaders accept client_id = NULL so setup_sync_bus() can pass the active
  # client context through to each S3 read when a staff member has jumped contexts.
  register_synced(
    "pagar_hoy_db",
    S3_KEYS$pagar_hoy_sync,
    # Global loader: returns shared data when sync is ON (all sessions see same data)
    function(client_id = NULL) {
      if (isTRUE(tryCatch(.GlobalEnv$.agenda_sync$is_on, error = function(e) FALSE)))
        .GlobalEnv$.agenda_sync$data
      else
        NULL  # per_session_loader handles sync-OFF case
    },
    # Per-session loader: returns the cached per-user data after a per-user save
    # or after sync is deactivated and per-user files have been written.
    per_session_loader = function(shared) {
      sync_on <- isTRUE(tryCatch(.GlobalEnv$.agenda_sync$is_on, error = function(e) FALSE))
      if (sync_on) return(NULL)
      ukey  <- tolower(trimws(tryCatch(shared$current_user(), error = function(e) "")))
      if (!nzchar(ukey)) return(NULL)
      cache <- tryCatch(.GlobalEnv$.agenda_user_cache, error = function(e) NULL)
      if (!is.list(cache)) return(NULL)
      cache[[ukey]]
    }
  )
  register_synced("bancos_confirmados_db", S3_KEYS$bancos_confirmados,
                  load_bancos_confirmados)
  register_synced("bancos_movimientos_db", S3_KEYS$bancos_movimientos,
                  function(client_id = NULL)
                    load_bancos_movimientos(include_deleted = TRUE,
                                            client_id       = client_id))
  register_synced("ctas_cuentas",          S3_KEYS$ctas_cuentas,
                  load_ctas_cuentas)
  register_synced("proveedores_db",        S3_KEYS$proveedores,
                  load_proveedores)
  register_synced("empresas_db",           S3_KEYS$empresas,
                  load_empresas)
  register_synced("grupos_db",             S3_KEYS$grupos,
                  load_grupos)
  register_synced("pasivos_provisions_db",  S3_KEYS$pasivos_provisions,
                  load_pasivos_provisions)
  register_synced("pasivos_liabilities_db", S3_KEYS$pasivos_liabilities,
                  load_pasivos_liabilities)
  register_synced("papelera_rv", S3_KEYS$papelera,        load_papelera)
  register_synced("notes_df",   S3_KEYS$notes,          load_notes)
  register_synced("tags_db",    S3_KEYS$tags,            load_tags)
  register_synced("moves_db",   S3_KEYS$moves,           load_moves)
  register_synced("policy_catalog_db", S3_KEYS$policy_catalog, load_policy_catalog)
  register_synced("forecasting_series_observations_db",
                  S3_KEYS$forecasting_series_observations,
                  load_forecasting_series_observations)
  register_synced("forecasting_metrics_db",
                  S3_KEYS$forecasting_metrics,
                  load_forecasting_metrics)
  register_synced("forecasting_subscriptions_db",
                  S3_KEYS$forecasting_subscriptions,
                  load_forecasting_subscriptions)
  register_synced("forecasting_manual_curves_db",
                  S3_KEYS$forecasting_manual_curves,
                  load_forecasting_manual_curves)
  register_synced("conciliacion_rv",    S3_KEYS$conciliacion,    load_conciliacion)
  register_synced("parte_alias_map_db", S3_KEYS$parte_alias_map, load_parte_alias_map)

  setup_sync_bus(session, shared, poll_ms = 8000,
                 active_client_rv = effective_client_id)

  # ── Abono parcial ─────────────────────────────────────────────────────────────
  setup_abono_browse(input, output, session,
                     sap_data, abonos_db, pagar_hoy_db, current_user,
                     client_id = shared$effective_client_id)

  # ── Pasivos lifecycle observers and module ────────────────────────────────────
  setup_pasivos_observers(input, output, session, shared)
  setup_pasivos_module(input, output, session, shared)
  pasivos_table_module_server("pt", shared)

  # ── Stage 4: wizard, edit-confirm ─────────────────────────────────────────
  setup_pasivos_wizard(input, output, session, shared)
  setup_pasivos_edit_confirm(input, output, session, shared)

  # ── Stage 6.1: Forecasting module ─────────────────────────────────────────
  forecastingServer("fc", shared)

  # ── Show GRUPO menu and control per-panel visibility ─────────────────────────
  # GRUPO toggle is hidden by default via CSS (display:none!important).
  # Empresas  → dev + admin  (can_manage_empresas permission, TRUE for both by default).
  # Usuarios  → dev only; hidden from admin's dropdown so they don't see the option.
  observeEvent(shared$current_user_info(), {
    tier <- tryCatch(shared$current_user_info()$tier, error = function(e) "")
    if (!tier %in% c("principal", "hopdesk", "dev", "admin")) return()

    # Reveal the GRUPO dropdown toggle (overrides CSS !important)
    shinyjs::runjs("
      document.querySelectorAll('.navbar .nav-link[data-value=\"GRUPO\"]')
        .forEach(function(el) {
          el.style.setProperty('display', 'flex', 'important');
          var li = el.closest('li');
          if (li) li.style.setProperty('display', 'list-item', 'important');
        });
    ")

    if (tier %in% c("principal", "hopdesk", "dev")) {
      # Principal, Hopdesk and Dev see both dropdown items — ensure Usuarios item is visible
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

  # ── Admin deployment: hide financial tabs until a client context is active ────
  # Uses runjs with setProperty(...,'important') to override the initial CSS rule
  # that also uses !important — shinyjs::show/hide cannot beat !important stylesheets.
  if (IS_ADMIN_DEPLOYMENT) {
    FINANCIAL_TABS <- c("CAL", "VEN", "PH", "BNC", "IC", "PSV", "FC", "RPT")

    observe({
      tier <- tryCatch(shared$current_user_info()$tier, error = function(e) "")
      # Hopdesk staff only see financial modules when jumped into a client
      # context. All other tiers are clients and always see them.
      is_staff      <- isTRUE(tryCatch(shared$current_user_info()$is_staff, error = function(e) FALSE))
      in_client_ctx <- !is.null(tryCatch(jump_client_id(), error = function(e) NULL))
      show_tabs     <- if (is_staff) in_client_ctx else nzchar(tier)

      lapply(FINANCIAL_TABS, function(tab) {
        if (show_tabs) {
          shinyjs::runjs(sprintf("
            document.querySelectorAll('.navbar .nav-link[data-value=\"%s\"]').forEach(function(el) {
              el.style.setProperty('display', 'flex', 'important');
              var li = el.closest('li');
              if (li) li.style.removeProperty('display');
            });
          ", tab))
        } else {
          shinyjs::runjs(sprintf("
            document.querySelectorAll('.navbar .nav-link[data-value=\"%s\"]').forEach(function(el) {
              el.style.setProperty('display', 'none', 'important');
              var li = el.closest('li');
              if (li) li.style.setProperty('display', 'none', 'important');
            });
          ", tab))
        }
      })
    })

    # After login: staff sessions get moved off the default CAL landing tab to
    # TIERS (Usuarios, inside the Grupo dropdown) — "GRUPO" itself is a
    # bslib::nav_menu (dropdown container), not a selectable nav_panel, so it
    # cannot be passed to updateNavbarPage(); TIERS is its real child panel.
    # Client sessions do nothing — they're already on CAL. Uses a manual
    # guard flag instead of observeEvent(..., once = TRUE): once + req() has
    # ambiguous timing if req() aborts before info$user is genuinely resolved,
    # which is the mechanism behind the tesoreria routing incident.
    redirected_to_staff_home <- reactiveVal(FALSE)
    observe({
      info <- current_user_info()
      if (isTRUE(redirected_to_staff_home())) return()      # already handled
      if (is.null(info) || info$user == "unknown") return() # not resolved yet
      redirected_to_staff_home(TRUE)                         # mark BEFORE acting
      if (isTRUE(info$is_staff)) updateNavbarPage(session, "nav", selected = "TIERS")
    })

    # Load hop grants once after login — needed immediately for context-jump
    # checks. Same once + req() fix as above: manual guard flag instead of
    # once = TRUE, which previously left hop_grants_db() permanently empty for
    # every session.
    hop_grants_loaded <- reactiveVal(FALSE)
    observe({
      info <- current_user_info()
      if (isTRUE(hop_grants_loaded())) return()
      if (is.null(info) || info$user == "unknown") return()
      hop_grants_loaded(TRUE)
      hg <- tryCatch(load_hop_grants(), error = function(e) NULL)
      if (!is.null(hg)) hop_grants_db(hg)
    }, priority = -2)
  }

  # ── Username badge in navbar ──────────────────────────────────────────────────
  output$navbar_user_badge <- renderUI({
    cid <- tryCatch(jump_client_id(), error = function(e) NULL)  # dep — re-renders on every jump
    info  <- shared$current_user_info()
    user  <- info$user %||% ""
    if (!nzchar(user) || user == "unknown") return(NULL)
    name  <- info$name %||% user
    tier  <- info$tier %||% "finance"
    in_jump <- !is.null(cid)

    tier_color <- switch(tier,
      principal = "#7b1fa2",
      hopdesk   = "#c2185b",
      dev       = "#0d1b3e",
      admin     = "#6610f2",
      finance   = "#0a58ca",
      analysis  = "#6c757d",
      "#6c757d"
    )

    client_pill <- if (in_jump)
      tags$span(
        toupper(cid),
        style = "font-size:.65rem; font-weight:700; letter-spacing:1px; text-transform:uppercase;
                 background:#e65100; border:1px solid rgba(255,255,255,.35);
                 padding:2px 7px; border-radius:20px; margin-left:4px;"
      )
    else NULL

    tags$span(
      style = "display:flex; align-items:center; gap:6px; margin:4px 4px 4px 0; font-size:.8rem; opacity:.9; color:#fff;",
      icon("user-circle"),
      tags$span(name),
      tags$span(tier, style = sprintf(
        "font-size:.65rem; font-weight:700; letter-spacing:1px; text-transform:uppercase;
         background:%s; border:1px solid rgba(255,255,255,.35); padding:2px 7px; border-radius:20px;",
        tier_color
      )),
      client_pill
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
    message(sprintf("[ERP_DATA] written at t+%.1fs — AR=%d rows  AP=%d rows (session #%d)%s%s",
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
    # A staff session at home (not jumped into any client) has no ERP to
    # fetch — hd-admin has no SAP credentials and no companies. Skip the
    # fetch entirely rather than running it just to get an empty result.
    if (isTRUE(current_user_info()$is_staff) && is.null(jump_client_id())) {
      message(sprintf("[SAP] Skipping fetch — staff session at home, nothing to fetch (session #%d)", .sn))
      return(invisible(NULL))
    }
    message(sprintf("[ERP] load_erp_data() starting at t+%.1fs (session #%d)",
                    (proc.time() - .t_session)[["elapsed"]], .sn))
    if (.sap_running)               return(invisible(NULL))
    if (.sap_ever_done && !force)   return(invisible(NULL))

    .sap_running <<- TRUE
    sap_loading(TRUE)
    session$sendCustomMessage("refresh_start", list())
    showNotification("Conectando al ERP…", id = "erp_load",
                     duration = NULL, type = "message")
    on.exit({
      .sap_running   <<- FALSE
      sap_loading(FALSE)
      session$sendCustomMessage("refresh_end", list())
      removeNotification("erp_load")
    })

    load_ledger <- function(ledger_name) {
      result <- tryCatch(
        fetch_all_companies(ledger_name, isolate(company_map_rv()),
                            client_id = isolate(effective_client_id())),
        sap_no_connection = function(e) {
          # Transport failure (VPN/network) — fall through to snapshot silently;
          # the snapshot block below shows the dated "SAP no disponible" notification.
          NULL
        },
        error = function(e) {
          showNotification(paste("SAP", ledger_name, ":", e$message), type = "warning")
          NULL
        }
      )
      if (!is.null(result)) {
        info <- sap_snapshot_info()
        info[[ledger_name]] <- list(ts = Sys.time(), is_live = TRUE)
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
        info[[ledger_name]] <- list(ts = snap$saved_at, is_live = FALSE)
        sap_snapshot_info(info)
        return(snap$data)
      }
      NULL
    }

    t0_ar <- proc.time()
    message(sprintf("[ERP] Fetching AR from ERP at t+%.1fs (session #%d)",
                    (proc.time() - .t_session)[["elapsed"]], .sn))
    ar <- load_ledger("AR")
    message(sprintf("[ERP] AR done in %.1fs — starting AP (session #%d)",
                    (proc.time() - t0_ar)[["elapsed"]], .sn))
    t0_ap <- proc.time()
    ap <- load_ledger("AP")
    message(sprintf("[ERP] AP done in %.1fs — total fetch %.1fs (session #%d)",
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
    # session #2 should use them rather than re-fetching, but ONLY a session
    # in the same client context — keyed by client id so this can never seed
    # a different client's (or a staff-at-home session's) sap_data().
    .sap_cache_set(effective_client_id(), ar, ap)
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

      # Load SAP snapshots + policy_moves together so the first calendar render
      # already uses policy-adjusted dates for all previously-seen invoices.
      t0_snap <- proc.time()
      ar_snap <- tryCatch(load_sap_snapshot("AR"), error = function(e) NULL)
      ap_snap <- tryCatch(load_sap_snapshot("AP"), error = function(e) NULL)
      pm_snap <- tryCatch(load_policy_moves(),     error = function(e) NULL)
      message(sprintf("[LOAD] SAP snapshots + policy_moves fetched in %.1fs",
                      (proc.time() - t0_snap)[["elapsed"]]))

      if (!is.null(pm_snap)) policy_moves_db(pm_snap)

      if (!is.null(ar_snap) || !is.null(ap_snap)) {
        sap_data(list(
          AR = if (!is.null(ar_snap)) ar_snap$data else NULL,
          AP = if (!is.null(ap_snap)) ap_snap$data else NULL
        ))
        sap_snapshot_info(list(
          AR = if (!is.null(ar_snap)) list(ts = ar_snap$saved_at, is_live = FALSE) else NULL,
          AP = if (!is.null(ap_snap)) list(ts = ap_snap$saved_at, is_live = FALSE) else NULL
        ))
      }

      message(sprintf(
        "[LOAD] Phase 1 done in %.1fs — handing off to CUR_CHOICES (p1) + SAP dispatch (p-1), Phase 2 queued (p-2)",
        (proc.time() - t0)[["elapsed"]]
      ))
      .s3_load_complete(TRUE)
      .phase1_done(TRUE)
      session$sendCustomMessage("loadingProgress", list(pct = 45))
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
      t0     <- proc.time()
      # Resolve the home client prefix set at login (before Phase 1 — no jump
      # can have happened yet). Inside isolate() this reads the current value
      # without creating a reactive dependency — Phase 2 fires once, at
      # startup; the Stage 4.4 observer below is what reloads on a jump.
      cid_lo <- tolower(home_client_id() %||% Sys.getenv("CLIENT_ID"))
      message(sprintf("[LOAD] Phase 2 start at t+%.1fs (session #%d) [cid=%s]",
                      (proc.time() - .t_session)[["elapsed"]], .sn, cid_lo))

      # Batch-fetch all deferred S3 keys not already in the preload cache
      local({
        t0_fetch <- proc.time()
        message("[LOAD] Fetching deferred S3 objects...")
        # keys_needed: logical names whose file-suffix keys are not yet in cache.
        # Cache keyed by file suffix (e.g. "interco_v2.rds") to match .s3_read().
        # "notes" and "sync_versions" are intentionally excluded from the preload
        # cache so every load_notes() call (Phase 2, sync bus, panel toggle) always
        # hits S3 directly.  This ensures one session's Phase-2 cache hit can never
        # serve stale data to another session that started before a note was saved.
        SKIP_PRELOAD <- c("sync_versions", "notes")
        # Check for already-cached full-prefix keys (e.g. "networks/movimientos.rds")
        # set by a previous session for the same client.
        keys_needed <- names(S3_KEYS)[
          !vapply(paste0(cid_lo, "/", unname(S3_KEYS)), exists,
                  logical(1), envir = .s3_preload_cache, inherits = FALSE) &
          !names(S3_KEYS) %in% SKIP_PRELOAD
        ]
        if (length(keys_needed)) {
          bucket <- .s3_bucket()
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
            # Store under full prefix key so .s3_read_with() gets a cache hit
            # without cross-client collisions in the shared global cache.
            full_key <- paste0(cid_lo, "/", S3_KEYS[[keys_needed[i]]])
            assign(full_key, raw[[i]], envir = .s3_preload_cache)
          }
          message(sprintf("[LOAD] %d deferred objects fetched in %.1fs",
                          length(keys_needed),
                          (proc.time() - t0_fetch)[["elapsed"]]))
        } else {
          message("[LOAD] All S3 keys already cached")
        }
      })

      # Load aux objects with per-item timing (all guaranteed cache hits above)
      t1 <- proc.time(); moves_db(tryCatch(load_moves(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   moves            %.1fs", (proc.time() - t1)[["elapsed"]]))
      # policy_moves_db is loaded in Phase 1 so the first calendar render already
      # has policy-adjusted dates. Only load here as a fallback when Phase 1 failed.
      t1 <- proc.time()
      if (is.null(policy_moves_db()))
        policy_moves_db(tryCatch(load_policy_moves(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   policy_moves     %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); policy_catalog_db(tryCatch(load_policy_catalog(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   policy_catalog   %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); partner_policies_db(tryCatch(load_partner_policies(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   partner_policies %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); holiday_overrides_db(tryCatch(load_holiday_overrides(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   holiday_overrides %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); notes_df(tryCatch(load_notes(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   notes            %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); tags_db(tryCatch(load_tags(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   tags             %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); manual_inv(tryCatch(load_manual(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   manual           %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); sap_ov_db(tryCatch(load_sap_overrides(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   sap_overrides    %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      interco_v2(tryCatch(load_interco_v2(client_id = cid_lo), error = function(e) NULL) %||%
                   list(ar_prefix = "C", ap_prefix = "P", companies = list()))
      message(sprintf("[LOAD]   interco_v2       %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      local({
        cfg <- tryCatch(load_agenda_sync_config(client_id = cid_lo),
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
          ph <- tryCatch(load_pagar_hoy(client_id = cid_lo), error = function(e) .schema_pagar_hoy())
          ph <- .merge_early_adds(ph)
          tryCatch(save_pagar_hoy_sync(ph, client_id = cid_lo), error = function(e)
            message("[LOAD] agenda_sync migrate write failed: ", e$message))
          tryCatch(save_agenda_sync_config(TRUE, "system", client_id = cid_lo), error = function(e) NULL)
          .GlobalEnv$.agenda_sync$is_on   <- TRUE
          .GlobalEnv$.agenda_sync$data    <- ph
          .GlobalEnv$.agenda_sync$version <- as.character(Sys.time())
          pagar_hoy_db(ph)
        } else if (isTRUE(cfg$is_enabled)) {
          .GlobalEnv$.agenda_sync$is_on <- TRUE
          # Always read from S3 (via preload cache — already fetched in batch above)
          # on every new session login.  The in-memory $data was intentionally NOT
          # used as the load source here: it persists across sessions in the same
          # R process, so a week-old value would silently shadow every new login
          # even after the user cleared the agenda and the S3 file was updated.
          # $data is refreshed below and remains available for save_pagar_hoy /
          # the poll observer to provide real-time cross-session sync.
          d <- tryCatch(load_pagar_hoy_sync(client_id = cid_lo), error = function(e) .schema_pagar_hoy())
          .GlobalEnv$.agenda_sync$data    <- d
          .GlobalEnv$.agenda_sync$version <- as.character(Sys.time())
          ph <- .merge_early_adds(d)
          if (!identical(ph, d)) {
            # Early-add items were merged in — persist immediately
            tryCatch(save_pagar_hoy(ph, current_user(), client_id = cid_lo), error = function(e) NULL)
          }
          pagar_hoy_db(ph)
        } else {
          .GlobalEnv$.agenda_sync$is_on <- FALSE
          ph <- tryCatch(
            load_pagar_hoy_user(current_user(), client_id = cid_lo),
            error = function(e) tryCatch(load_pagar_hoy(client_id = cid_lo), error = function(e2) NULL))
          ph <- .merge_early_adds(ph %||% .schema_pagar_hoy())
          pagar_hoy_db(ph)
        }
      })
      message(sprintf("[LOAD]   pagar_hoy        %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); abonos_db(tryCatch(load_abonos(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   abonos           %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); papelera_rv(tryCatch(load_papelera(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   papelera         %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); proveedores_db(tryCatch(load_proveedores(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   proveedores      %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      proveedores_inactivos_db(tryCatch(load_proveedores_inactivos(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   prov_inactivos   %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); ctas_cuentas(tryCatch(load_ctas_cuentas(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   ctas_cuentas     %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      bancos_cuentas_db(tryCatch(load_bancos_cuentas(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   bancos_cuentas   %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      bancos_movimientos_db(tryCatch(
        load_bancos_movimientos(include_deleted = TRUE, client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   bancos_movs      %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      bancos_confirmados_db(tryCatch(load_bancos_confirmados(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   bancos_confirm   %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); conciliacion_rv(tryCatch(load_conciliacion(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   conciliacion     %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time(); parte_alias_map_db(tryCatch(load_parte_alias_map(client_id = cid_lo), error = function(e) NULL))
      message(sprintf("[LOAD]   parte_alias_map  %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      local({
        provs <- tryCatch(load_pasivos_provisions(client_id = cid_lo), error = function(e) NULL)
        if (!is.null(provs)) pasivos_provisions_db(provs)
      })
      message(sprintf("[LOAD]   pasivos_provs    %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      local({
        liabs <- tryCatch(load_pasivos_liabilities(client_id = cid_lo), error = function(e) NULL)
        if (!is.null(liabs)) pasivos_liabilities_db(liabs)
      })
      message(sprintf("[LOAD]   pasivos_liabs    %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      local({
        obs <- tryCatch(load_forecasting_series_observations(client_id = cid_lo), error = function(e) NULL)
        if (!is.null(obs)) forecasting_series_observations_db(obs)
      })
      message(sprintf("[LOAD]   fc_observations  %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      local({
        mt <- tryCatch(load_forecasting_metrics(client_id = cid_lo), error = function(e) NULL)
        if (!is.null(mt)) forecasting_metrics_db(mt)
      })
      message(sprintf("[LOAD]   fc_metrics       %.1fs", (proc.time() - t1)[["elapsed"]]))
      t1 <- proc.time()
      local({
        sb <- tryCatch(load_forecasting_subscriptions(client_id = cid_lo), error = function(e) NULL)
        if (!is.null(sb)) forecasting_subscriptions_db(sb)
      })
      message(sprintf("[LOAD]   fc_subscriptions %.1fs", (proc.time() - t1)[["elapsed"]]))
      # Seed catalogs on first run (idempotent — no-op if already populated).
      tryCatch(forecasting_seed_if_empty(), error = function(e)
        warning("[LOAD] forecasting_seed_if_empty: ", conditionMessage(e)))

      t1 <- proc.time()
      local({
        grp <- tryCatch(load_grupos(client_id = cid_lo), error = function(e) NULL)
        if (!is.null(grp)) grupos_db(grp)
      })
      message(sprintf("[LOAD]   grupos           %.1fs", (proc.time() - t1)[["elapsed"]]))

      # empresas_db is not handled by the context-switch observer for the initial
      # login event (ignoreInit = TRUE suppresses the first jump_client_id fire,
      # and login itself never sets jump_client_id). Load it here in Phase 2 so
      # it is always available after startup.
      t1 <- proc.time()
      local({
        emp <- tryCatch(load_empresas(client_id = cid_lo), error = function(e) NULL)
        if (!is.null(emp)) empresas_db(emp)
      })
      message(sprintf("[LOAD]   empresas         %.1fs", (proc.time() - t1)[["elapsed"]]))

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
      session$sendCustomMessage("loadingProgress", list(pct = 100))
      # .cal_fade_pending(TRUE)  # OVERLAY FADE — abandoned; see observer below
    })
  }, priority = -2)

  # Reads a SAP snapshot from an arbitrary client prefix, bypassing .s3_key()
  # (which always reads from the native CLIENT_ID folder).
  .load_sap_snapshot_for_client <- function(ledger, client_id) {
    key      <- if (toupper(ledger) == "AR") S3_KEYS_CRITICAL$sap_snap_ar else S3_KEYS_CRITICAL$sap_snap_ap
    full_key <- paste0(tolower(client_id), "/", key)
    tryCatch(
      aws.s3::s3readRDS(object = full_key, bucket = .s3_bucket()),
      error = function(e) NULL
    )
  }

  # ── Stage 4.4 / Stage 2 Part A: context-switch reload ─────────────────────────
  # Fires when jump_client_id() changes — both jumping into a client AND
  # jumping back to home (ignoreNULL = FALSE) — and reloads every financial
  # reactiveVal, including the SAP snapshot, from effective_client_id()'s S3
  # prefix. This observer runs AFTER Phase 2 (priority -3). Never fires for a
  # client session (jump_client_id() is never settable by a non-staff session
  # — enforced in tiers_module.R's context switcher).
  observeEvent(jump_client_id(), {
    jcid          <- jump_client_id()
    effective_cid <- effective_client_id()

    message(sprintf("[CTX] Switching to client context '%s'", effective_cid))

    .ctx_load <- function(fn, rv, label) {
      errored <- FALSE
      result  <- tryCatch(fn(client_id = effective_cid), error = function(e) {
        message(sprintf("[CTX]   %s WARN: %s", label, e$message))
        errored <<- TRUE
        NULL
      })
      if (!errored) {
        rv(result)   # NULL = client has no data — clears previous client's reactive
        message(sprintf("[CTX]   %s OK", label))
      }
    }

    .ctx_load(load_bancos_confirmados,            bancos_confirmados_db,               "bancos_confirmados")
    .ctx_load(load_ctas_cuentas,                  ctas_cuentas,                        "ctas_cuentas")
    .ctx_load(function(client_id) load_bancos_movimientos(include_deleted = TRUE, client_id = client_id),
                                                  bancos_movimientos_db,               "bancos_movimientos")
    .ctx_load(load_proveedores,                   proveedores_db,                      "proveedores")
    .ctx_load(load_empresas,                      empresas_db,                         "empresas")
    .ctx_load(load_grupos,                        grupos_db,                           "grupos")
    .ctx_load(load_pasivos_provisions,            pasivos_provisions_db,               "pasivos_provisions")
    .ctx_load(load_pasivos_liabilities,           pasivos_liabilities_db,              "pasivos_liabilities")
    .ctx_load(load_papelera,                      papelera_rv,                         "papelera")
    .ctx_load(load_notes,                         notes_df,                            "notes")
    .ctx_load(load_tags,                          tags_db,                             "tags")
    .ctx_load(load_moves,                         moves_db,                            "moves")
    .ctx_load(load_policy_catalog,                policy_catalog_db,                   "policy_catalog")
    .ctx_load(load_forecasting_series_observations, forecasting_series_observations_db,"forecasting_obs")
    .ctx_load(load_forecasting_metrics,           forecasting_metrics_db,              "forecasting_metrics")
    .ctx_load(load_forecasting_subscriptions,     forecasting_subscriptions_db,        "forecasting_subs")
    .ctx_load(load_forecasting_manual_curves,     forecasting_manual_curves_db,        "forecasting_curves")
    .ctx_load(load_conciliacion,                  conciliacion_rv,                     "conciliacion")
    .ctx_load(load_parte_alias_map,               parte_alias_map_db,                  "parte_alias_map")
    .ctx_load(load_manual,                        manual_inv,                          "manual_inv")
    .ctx_load(load_abonos,                        abonos_db,                           "abonos")
    .ctx_load(load_sap_overrides,                 sap_ov_db,                           "sap_overrides")
    # Reactives that Phase 2 loads from the env-var prefix but the context switch
    # must reload from the active client's prefix.
    .ctx_load(load_bancos_cuentas,        bancos_cuentas_db,        "bancos_cuentas")
    .ctx_load(load_partner_policies,      partner_policies_db,      "partner_policies")
    .ctx_load(load_holiday_overrides,     holiday_overrides_db,     "holiday_overrides")
    .ctx_load(load_policy_moves,          policy_moves_db,          "policy_moves")
    .ctx_load(load_proveedores_inactivos, proveedores_inactivos_db, "prov_inactivos")
    local({
      cfg_ctx <- tryCatch(load_agenda_sync_config(client_id = effective_cid),
                          error = function(e) list(is_enabled = FALSE))
      if (isTRUE(cfg_ctx$is_enabled)) {
        ph <- tryCatch(load_pagar_hoy_sync(client_id = effective_cid),
                       error = function(e) NULL)
        if (!is.null(ph)) {
          .GlobalEnv$.agenda_sync$is_on   <- TRUE
          .GlobalEnv$.agenda_sync$data    <- ph
          .GlobalEnv$.agenda_sync$version <- as.character(Sys.time())
        }
      } else {
        .GlobalEnv$.agenda_sync$is_on <- FALSE
        ph <- tryCatch(
          load_pagar_hoy_user(current_user(), client_id = effective_cid),
          error = function(e) tryCatch(
            load_pagar_hoy(client_id = effective_cid),
            error = function(e2) NULL))
      }
      if (!is.null(ph)) { pagar_hoy_db(ph); message("[CTX]   pagar_hoy OK") }
    })
    # interco_v2 uses a cache keyed to the native deployment — bypass it with client_id.
    # Wrap result so NULL (no IC config for this client) becomes the empty struct.
    local({
      result <- tryCatch(load_interco_v2(client_id = effective_cid), error = function(e) {
        message(sprintf("[CTX]   interco_v2 WARN: %s", e$message)); NULL
      })
      interco_v2(result %||% list(ar_prefix = "C", ap_prefix = "P", rfcs = character(), companies = list()))
      message("[CTX]   interco_v2 OK")
    })
    .ctx_load(load_group_config,                  group_config_rv,                     "group_config")

    # Load SAP snapshots from jumped client — always overwrite so jumping to an
    # empty client clears the calendar (no guard: NULL = no data for this client).
    snap_ar  <- .load_sap_snapshot_for_client("AR", effective_cid)
    snap_ap  <- .load_sap_snapshot_for_client("AP", effective_cid)
    new_snap <- list(
      AR = if (!is.null(snap_ar)) snap_ar$data else NULL,
      AP = if (!is.null(snap_ap)) snap_ap$data else NULL
    )
    sap_data(new_snap)
    # Update snapshot info so the orange pill indicator reflects the jumped client.
    # is_live is always FALSE for context-jump data (we never hit live SAP for jumped clients).
    sap_snapshot_info(list(
      AR = if (!is.null(snap_ar)) list(ts = snap_ar$saved_at, is_live = FALSE) else NULL,
      AP = if (!is.null(snap_ap)) list(ts = snap_ap$saved_at, is_live = FALSE) else NULL
    ))
    # Seed the cross-session SAP cache so subsequent sessions on the same R
    # process AND ON THE SAME CLIENT get the client's snapshot immediately —
    # keyed by effective_cid so this can never seed a different client's (or
    # a staff-at-home session's) lookup.
    if (!is.null(new_snap$AR) || !is.null(new_snap$AP))
      .sap_cache_set(effective_cid, new_snap$AR, new_snap$AP)
    message(sprintf("[CTX]   sap_data OK (AR=%d AP=%d rows)",
                    nrow(new_snap$AR %||% data.frame()), nrow(new_snap$AP %||% data.frame())))

    # Dual audit log — only when jumping TO another client, not when resetting home
    if (!is.null(jcid)) {
      log_action(user        = current_user() %||% "unknown",
                 module      = "clientes",
                 action      = "client_access",
                 description = sprintf("Staff switched active context to '%s'", effective_cid),
                 target_id   = effective_cid,
                 s3_key      = NA_character_,
                 client_id   = "hd-admin",
                 viewer_home_client_id = home_client_id())
      log_action(user        = current_user() %||% "unknown",
                 module      = "clientes",
                 action      = "external_access",
                 description = "HopDesk staff accessed this client's data (session context switch)",
                 target_id   = effective_cid,
                 s3_key      = NA_character_,
                 client_id   = effective_cid,
                 viewer_home_client_id = home_client_id())
    }

    message(sprintf("[CTX] Context switch to '%s' complete", effective_cid))
  }, ignoreInit = TRUE, ignoreNULL = FALSE, priority = -3)

  # ── OVERLAY FADE TIMING — ALL APPROACHES ABANDONED ──────────────────────────
  #
  # Goal: send session$sendCustomMessage("calFadeNow", list()) at the exact moment
  # the AR calendar is fully settled so the loading screen fades cleanly.
  # The JS calFadeNow handler in overlay_init.js section 9 does a double-rAF fade.
  # The 30 s fallback in section 8 (loadingProgress handler) is now the only thing
  # that actually fades the overlay.
  #
  # WHAT WE TRIED (in order):
  #
  # 1. shinyjs::delay(2000) after Phase 2
  #    observeEvent(.cal_fade_pending(), {
  #      if (!isTRUE(.cal_fade_pending())) return()
  #      .cal_fade_pending(FALSE)
  #      shinyjs::delay(2000, { session$sendCustomMessage("calFadeNow", list()) })
  #    }, ignoreInit = TRUE)
  #    FAILED: 2 s was too short.  Phase 2 triggers CUR_CHOICES → AR render #1,
  #    then a second cascade fires ~8–10 s later (empresa/IC).  calFadeNow arrived
  #    before the cascade, overlay faded, cascade blanked the calendar.
  #
  # 2. MutationObserver in JS (2 s DOM-quiet + .cal-outer present)
  #    calFadeNow handler watched #ar-calendar via MutationObserver and faded only
  #    after 2 s of quiet.  Still failed: when calFadeNow arrived just after render
  #    #1, the observer saw 2 s of quiet (render #1 looked settled) and faded before
  #    render #2 arrived and blanked the calendar.
  #
  # 3. Server-side render counter via Shiny.setInputValue('cal_ar_rendered', ...)
  #    Added the setInputValue call to the AR renderUI script in ledger_module.R and
  #    counted arrivals in an observer here.
  #    n >= 2: a second AR render never arrived in practice — the console stopped at
  #            [OVERLAY] cal_ar_rendered #1 — so the 30 s fallback was the only fade.
  #    n >= 1: overlay was already hidden before the browser round-trip completed;
  #            something in the JS was hiding it even earlier (root cause unconfirmed).
  #
  # Nothing left to try that isn't just a longer timer in disguise.
  # The overlay_init.js 30 s fallback is now the active fade mechanism.
  # Do NOT add code here without first understanding why the browser-side hides
  # the overlay before server-side signals even arrive.
  #
  # Reactive vals that supported this (.cal_fade_pending, .ar_post_phase2_count)
  # are commented out above.  The setInputValue call in ledger_module.R is also gone.

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
    if (isTRUE(current_user_info()$is_staff) && is.null(jump_client_id())) {
      message(sprintf("[SAP] Skipping dispatch — staff session at home, nothing to fetch (session #%d)", .sn))
      .sap_ever_done <<- TRUE
      return()
    }
    message(sprintf("[SAP] sap_trigger fired at t+%.1fs (session #%d)",
                    (proc.time() - .t_session)[["elapsed"]], .sn))
    # Keyed by this session's own client id — never reads any other client's
    # entry, so a staff-at-home session can never inherit a client's data and
    # vice versa.
    cache <- .sap_cache_get(effective_client_id())
    if (.sap_cache_fresh(cache)) {
      message("[SAP] Cross-session cache hit for '", effective_client_id(), "' — skipping live fetch")
      if (!identical(sap_data()$AR, cache$AR) || !identical(sap_data()$AP, cache$AP))
        sap_data(list(AR = cache$AR, AP = cache$AP))
      .sap_ever_done <<- TRUE
      return()
    }
    message(sprintf("[SAP] No fresh cross-session cache for '%s' — proceeding to live fetch (session #%d)",
                    effective_client_id(), .sn))
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

    # Force-reload S3 bancos data unconditionally so any edits made by other
    # concurrent users are visible immediately — independent of ERP connectivity.
    tryCatch({
      shared$bancos_movimientos_db(load_bancos_movimientos(include_deleted = TRUE))
      shared$bancos_confirmados_db(load_bancos_confirmados())
      shared$conciliacion_rv(load_conciliacion())
    }, error = function(e)
      showNotification(paste0("Error al leer desde S3: ", e$message),
                       type = "warning", duration = 6)
    )

    load_sap_data(force = TRUE)
  })
  autoInvalidate <- reactiveTimer(30 * 60 * 1000)
  observeEvent(autoInvalidate(), { load_sap_data() })

  # ── Cash Flow Word export ─────────────────────────────────────────────────
  setup_cashflow_export_server(input, output, session, shared)

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

    # Returns list(dt, is_live) using the latest of AR/AP timestamps, or NULL.
    company_snap_lbl <- function() {
      entries <- Filter(Negate(is.null), list(snap_inf$AR, snap_inf$AP))
      if (!length(entries)) return(NULL)
      latest <- entries[[which.max(sapply(entries, function(x) as.numeric(x$ts)))]]
      list(
        dt      = format(latest$ts, "%d/%m/%Y %H:%M",
                         tz = (input$client_tz %||% "America/Mexico_City")),
        is_live = isTRUE(latest$is_live)
      )
    }

    cmap    <- company_map_rv()
    tip_inf <- company_snap_lbl()   # shared across all pills (same SAP timestamp)

    out <- div(
      class = "d-flex gap-1 align-items-center",
      lapply(names(cmap), function(initials) {
        nombre   <- cmap[[initials]]
        active   <- nombre %in% sel
        is_snap  <- !is.null(tip_inf) && !tip_inf$is_live
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

        if (!is.null(tip_inf)) {
          div(class = "emp-tooltip-wrap",
            btn,
            tags$div(class = "emp-tooltip",
              if (!tip_inf$is_live)
                tags$div(style = "font-weight:700; margin-bottom:2px;", "Snapshot"),
              tags$div(paste0("\u00daltima actualizaci\u00f3n: ", tip_inf$dt))
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
                                      value = Sys.Date(), autoClose = TRUE,
                                      addon = "none")
  })

  output$currency_ui <- renderUI({
    t0_cur_ui  <- proc.time()
    tab        <- input$nav %||% "CAL"
    ledger_tab <- if (tab == "CAL") active_cal_ledger() else "AR"
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
  ar_state <- ledgerModuleServer("ar", config = AR_CONFIG, shared = shared)
  message(sprintf("[SERVER] ledger AR registered in %.2fs (session #%d)",
                  (proc.time() - t0_mod)[["elapsed"]], .sn))
  t0_mod <- proc.time()
  ap_state <- ledgerModuleServer("ap", config = AP_CONFIG, shared = shared)
  # Expose the fully-filtered, pre-month-filter data to Search and Vencidos so
  # they are guaranteed to show exactly what the calendar knows about.
  shared$df_combined_AR <- ar_state$df_combined
  shared$df_combined_AP <- ap_state$df_combined
  cashflowPreviewServer("cfp", shared = shared)
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
          status    = "pending",
          source    = "manual"
        )
        ph_updated <- upsert_pagar_hoy(
          pagar_hoy_db() %||% safe_load_pagar_hoy(current_user(), client_id = effective_client_id()),
          ph_row, keys = "id")
        pagar_hoy_db(ph_updated)
        tryCatch(save_pagar_hoy(ph_updated, current_user(), client_id = effective_client_id()),
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

  # ── Business partner suggestions in Nueva entrada modal (AP and AR) ───────────
  me_prov_query   <- reactive({ input$me_parte %||% "" })
  me_prov_query_d <- debounce(me_prov_query, 300)

  output$me_prov_suggestions <- renderUI({
    ldr <- active_entry_ledger()
    req(ldr %in% c("AP", "AR"))
    q <- trimws(me_prov_query_d())
    if (nchar(q) < 2) return(NULL)

    if (ldr == "AP") {
      # ── AP: fuzzy match against proveedores catalog with IC badges ─────────
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

    } else {
      # ── AR: match against the same all-partes pool used by payment policies ─
      sap_d    <- tryCatch(shared$sap_data(),               error = function(e) list())
      pp_db    <- tryCatch(shared$partner_policies_db(),    error = function(e) NULL)
      pasiv_db <- tryCatch(shared$pasivos_liabilities_db(), error = function(e) NULL)

      from_sap <- c(
        if (!is.null(sap_d$AR) && "Parte" %in% names(sap_d$AR)) unique(sap_d$AR$Parte) else character(),
        if (!is.null(sap_d$AP) && "Parte" %in% names(sap_d$AP)) unique(sap_d$AP$Parte) else character()
      )
      from_pp <- if (!is.null(pp_db) && nrow(pp_db)) pp_db$parte else character()
      from_pasivos <- if (!is.null(pasiv_db) && nrow(pasiv_db) && "parte" %in% names(pasiv_db))
        unique(pasiv_db$parte[pasiv_db$estado %in% c("active", "paused") & nzchar(pasiv_db$parte %||% "")])
      else character()

      all_p   <- sort(unique(c(from_sap, from_pp, from_pasivos)))
      all_p   <- all_p[nzchar(trimws(all_p))]
      q_norm  <- strip_accents(tolower(q))
      matches <- head(all_p[grepl(q_norm, strip_accents(tolower(all_p)), fixed = TRUE)], 8L)
      if (!length(matches)) return(NULL)

      div(class = "prov-suggest-box",
        style = "border:1px solid #dee2e6; border-radius:6px; margin-top:-8px; margin-bottom:8px; overflow:hidden;",
        lapply(matches, function(nombre) {
          payload <- jsonlite::toJSON(
            list(nombre = nombre, alias = "", moneda = ""),
            auto_unbox = TRUE
          )
          tags$div(
            class       = "prov-suggest-item d-flex align-items-center gap-2 px-3 py-2",
            style       = "cursor:pointer; border-bottom:1px solid #f0f0f0; transition:background 0.1s;",
            onmouseover = "this.style.background='#e8eef8'",
            onmouseout  = "this.style.background=''",
            onclick     = paste0(
              "Shiny.setInputValue('me_prov_pick', ", payload, ", {priority:'event'})"
            ),
            tags$span(class = "fw-semibold flex-grow-1 small", nombre)
          )
        })
      )
    }
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
