# CLAUDIO3_SPEC.md
## HopDesk SaaS — Handoff Spec for Claudio 3

**For:** Claudio 3 (Claude Code agent — you have no memory of prior sessions)  
**Access:** Full read/write to `C:\Users\luisr\Antiguedad_App\`  
**Read this entire document before touching any code.**  
**Reference docs:**
- `CLAUDIO_SPEC.md` — original full architecture spec (design rationale, data schemas)
- `CLAUDIO2_SPEC.md` — previous handoff (the implementation Claudio 2 completed)

---

## 0. What You're Walking Into

This is an R Shiny SaaS financial operations platform for **Networks Group** (a logistics company). All persistent state is in AWS S3. Auth via `shinymanager`. No SQL database — every data structure is an `.rds` file in S3. One IAM key, one S3 bucket (`S3_BUCKET`), `CLIENT_ID` env var per deployment.

Claudio 1 built the SaaS foundation but broke the Settings module. Claudio 2 fixed the breakage, completed all 7 foundation stages, and passed 168/168 tests. **Do not revisit any of that — it is done.**

You have two tasks, in order:

1. **Fix the context jump** — staff switching to a client should see that client's full data
2. **hd-admin deployment UI** — when `CLIENT_ID=hd-admin`, the app should look and feel like an admin tool, not a financial calendar

---

## 1. Non-Negotiable Rules

Carry these forward from CLAUDIO2_SPEC.md:

1. **DO NOT split any module file.** Every `.R` file in `R/` must stay as a single file. The one exception (`R/settings/*.R`) is already done and fixed — do not touch it.
2. **DO NOT modify tests without running them first.** Run `source("tests/_run_saas.R")` before and after every change to `auth.R`, `persistence.R`, or `tiers_module.R`. 168/168 must stay green.
3. **`dev` account does not exist by design.** Do not re-add it.
4. **`send_email()` must never throw.** Always fire-and-forget.
5. **`hd-admin/` paths bypass `.s3_key()`.** Use direct `aws.s3::s3readRDS` with full `hd-admin/xxxx.rds` key.
6. **User IDs (UUID) must never appear in rendered UI.**
7. **Permissions follow the user, not the client context.** When a staff member jumps to a client folder, their tier and permissions come from `hd-admin/usuarios.rds` unchanged. Do not reload or re-resolve permissions on context switch. The Grupo tab must remain accessible regardless of which client is currently active.

---

## 2. Codebase State — What Is Already Working

All of the following are **already implemented and must not be touched unless fixing a bug**:

- `R/auth.R` — SaaS-aware credential loading: loads both `<client_id>/usuarios.rds` AND `hd-admin/usuarios.rds` (staff overlay)
- `R/persistence.R` — `.s3_key(key, client_id=NULL)`, `.s3_read_with()`, emergency lock, username index, client registry, invite system, audit chunking
- `R/tiers_module.R` — full user management UI, client management UI (Clientes tab), permissions editor, context switcher modal, emergency lock UI (Seguridad tab)
- `R/app_audit.R` — chunked rotation, `s3_key` column, dual-write for jump context
- `R/email_service.R` — `send_email()`, sandbox mode, queue fallback
- `R/sync_bus.R` — cross-session version polling (loaders accept `client_id = NULL`)
- `R/settings/*.R` — split settings (8 files, sourced with `local=TRUE` in `global.R`)
- `tests/` — 168/168 SaaS tests passing

**The context-switch reload observer already exists in `app.R` at line 1667.** It reloads all financial reactiveVals from the jumped client's folder on `active_client_id()` change. Group config and SAP data are the two gaps.

---

## 3. Workstream A — Fix the Context Jump

The jump reloads financial data (bancos, pasivos, moves, empresas, etc.) but is missing two things: group config and SAP/calendar data. Without these, jumping to Networks Group shows empty branding and an empty calendar — not useful for staff.

### 3.1 Reload group config on context switch

**File:** `app.R` lines 1687–1706 (the `.ctx_load(...)` block inside `observeEvent(active_client_id(), ...)`)

Add one `.ctx_load` call after the existing list:

```r
.ctx_load(load_group_config, group_config_rv, "group_config")
```

`load_group_config` already accepts `client_id = NULL` — confirm at `R/persistence.R:1324`. No other change needed. When Mouse jumps to Networks, the group name and logo will update to reflect Networks Group.

### 3.2 Load SAP snapshot on context switch

**Background:** The SAP calendar (AR/AP) is the most important financial view for a staff member servicing a client. SAP snapshots are stored in the client's S3 folder as:
- `<client_id>/sap_snapshot_AR.rds`
- `<client_id>/sap_snapshot_AP.rds`

`load_sap_snapshot(ledger)` reads using `.s3_key()` which uses the env-var `CLIENT_ID` — so it always reads from the native deployment's folder. For jumped contexts we need to read directly.

**Add a helper function** in `app.R` (define it before the context-switch observer):

```r
.load_sap_snapshot_for_client <- function(ledger, client_id) {
  key <- if (toupper(ledger) == "AR") S3_KEYS_CRITICAL$sap_snap_ar else S3_KEYS_CRITICAL$sap_snap_ap
  full_key <- paste0(tolower(client_id), "/", key)
  tryCatch(
    aws.s3::s3readRDS(object = full_key, bucket = .s3_bucket()),
    error = function(e) NULL
  )
}
```

**In the context-switch observer**, after all the `.ctx_load(...)` calls and before the audit logging block, add:

```r
# Load SAP snapshots from jumped client so the calendar has data
snap_ar <- .load_sap_snapshot_for_client("AR", effective_cid)
snap_ap <- .load_sap_snapshot_for_client("AP", effective_cid)
if (!is.null(snap_ar) || !is.null(snap_ap)) {
  current_snap <- isolate(sap_data())
  new_snap <- list(
    AR = if (!is.null(snap_ar)) snap_ar$data else (current_snap$AR %||% NULL),
    AP = if (!is.null(snap_ap)) snap_ap$data else (current_snap$AP %||% NULL)
  )
  if (!is.null(new_snap$AR) || !is.null(new_snap$AP)) {
    sap_data(new_snap)
    message(sprintf("[CTX]   sap_data OK (AR=%d AP=%d rows)",
                    nrow(new_snap$AR %||% data.frame()), nrow(new_snap$AP %||% data.frame())))
  }
}
```

**When jumping back to home** (`effective_cid == env_cid`): the snapshot reads from the native folder which is correct. No special handling needed — the same block runs and reads from `env_cid/sap_snapshot_*.rds`.

### 3.3 Context indicator in the navbar

When in a jumped context, the user must always know which client they're viewing. Update the `output$navbar_user_badge` renderUI (already at `app.R:1216`) to include a client pill when `active_client_id()` is non-NULL:

```r
output$navbar_user_badge <- renderUI({
  info  <- shared$current_user_info()
  user  <- info$user %||% ""
  if (!nzchar(user) || user == "unknown") return(NULL)
  name  <- info$name %||% user
  tier  <- info$tier %||% "finance"
  cid   <- tryCatch(active_client_id(), error = function(e) NULL)
  home  <- tolower(Sys.getenv("CLIENT_ID"))
  in_jump <- !is.null(cid) && nzchar(cid) && tolower(cid) != home

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
```

The orange pill (`#e65100`) is a clear visual signal that this session is in a non-home client context.

### 3.4 Permissions — verify, do not change

**Already correct by design.** `current_user_info()` is populated once from `res_auth` (the shinymanager result) and does NOT reactively update on `active_client_id()` changes. It reads from `hd-admin/usuarios.rds` for staff. The Grupo tab visibility observer (`observeEvent(shared$current_user_info(), ...)`) runs once and is not affected by context switches. **No code change needed here.**

Confirm by grepping: `grep -n "current_user_info\|active_client_id" R/tiers_module.R` — the tier/permission reactives (`current_tier()`, `is_principal()`, `is_hopdesk()`) read from `current_user_info()` only, never from `active_client_id()`. If any permission reactive reads `active_client_id()`, that is a bug — fix it.

---

## 4. Workstream B — hd-admin Deployment UI

The `hd-admin` deployment is a **second ShinyApps.io deployment of the exact same codebase** with `CLIENT_ID=hd-admin` in its `.Renviron`. It is an internal tool for HopDesk staff (Mouse and Bunny). Financial modules are empty in the native `hd-admin/` context — the only financial data comes through the context jump. **The deployment name has not been decided yet** — stop and ask the user before deploying.

### 4.1 IS_ADMIN_DEPLOYMENT constant

**File:** `R/global.R` — add near the top of the file, after `readRenviron(".Renviron")`:

```r
IS_ADMIN_DEPLOYMENT <- tolower(trimws(Sys.getenv("CLIENT_ID"))) == "hd-admin"
```

This is the single gate for all admin-specific UI behavior. Do not replicate the string `"hd-admin"` elsewhere — always test `IS_ADMIN_DEPLOYMENT`.

### 4.2 Default tab

**File:** `app.R` — find the `bslib::navset_*` or `navbarPage` call that defines the top-level navigation. Locate the `selected` parameter (or the first `nav_panel` argument). When `IS_ADMIN_DEPLOYMENT` is TRUE, the default selected tab should be `"GRUPO"` (the management panel), not `"Calendario"`.

The exact change depends on how the navbar is built — read the UI section of `app.R` before making this change. Look for `selected =` in the nav definition and make it conditional:

```r
selected = if (IS_ADMIN_DEPLOYMENT) "GRUPO" else "Calendario"
```

### 4.3 Conditional financial tab visibility

When `IS_ADMIN_DEPLOYMENT` is TRUE and no client context is active, the financial tabs (Calendario, Vencidos, Agenda de Hoy, Bancos, Pasivos, Forecasting, Reporte) should be hidden. They should appear as soon as a client context is jumped into.

**Implementation approach:** Use a Shiny observer to show/hide nav panels based on `IS_ADMIN_DEPLOYMENT` and `active_client_id()`.

Add to `app.R` server(), after the GRUPO observer:

```r
# In admin deployment: hide financial tabs until a client context is active
if (IS_ADMIN_DEPLOYMENT) {
  FINANCIAL_TABS <- c("Calendario", "Vencidos", "Agenda", "Bancos",
                      "Pasivos", "Forecasting", "Reporte")

  observe({
    cid <- tryCatch(active_client_id(), error = function(e) NULL)
    in_client <- !is.null(cid) && nzchar(cid)

    lapply(FINANCIAL_TABS, function(tab) {
      if (in_client) shinyjs::show(selector = sprintf('[data-value="%s"]', tab))
      else           shinyjs::hide(selector = sprintf('[data-value="%s"]', tab))
    })
  })
}
```

**Note:** You must verify the exact `data-value` attribute names of the nav panels in the rendered HTML — they match the `title` or `value` argument of each `nav_panel()` call. Read the `app.R` UI section first to get them right.

### 4.4 Admin welcome panel

When in admin deployment with no client jumped (home context), the Grupo tab is the landing page. No additional "welcome" screen is needed — the existing Clientes tab and Usuarios tab in the Grupo dropdown provide the staff with their tools. The visual signal from the IS_ADMIN_DEPLOYMENT context (empty financial tabs + landing on Grupo) is sufficient.

### 4.5 Local testing of admin deployment

Before deploying to ShinyApps.io, test locally:

1. In `.Renviron`, temporarily change `CLIENT_ID=hd-admin`
2. `source("R/global.R")` then `shiny::runApp()`
3. Log in as Mouse (principal)
4. Verify: landing tab is Grupo, financial tabs are hidden
5. Jump to Networks via context switcher
6. Verify: financial tabs appear, calendar loads from Networks snapshot, group name updates to "Networks Group"
7. Jump back: financial tabs hide, group name reverts to hd-admin (no group_config there — falls back to schema default)
8. Restore `CLIENT_ID=networks` in `.Renviron`

---

## 5. What to Leave Alone

Do NOT touch these systems unless directly fixing a bug discovered during Workstream A/B testing:

- `R/auth.R` — SaaS credential loading is working
- `R/settings/*.R` — settings module is working
- `R/sync_bus.R` — version polling is working (don't add jump logic here — it already receives `active_client_rv`)
- All test files in `tests/` — run them but don't modify them
- `R/bancos_module.R`, `R/interco_module.R`, all other modules — leave alone
- Emergency lock, username registry, invite system, audit — all working; don't revisit

---

## 6. File Reference

| What | File | Key lines |
|------|------|-----------|
| Context-switch reload observer | `app.R` | ~1659–1727 |
| `active_client_id` reactiveVal | `app.R` | ~929 |
| `session_client_id` reactive | `app.R` | ~1149 |
| `navbar_user_badge` renderUI | `app.R` | ~1216 |
| `IS_ADMIN_DEPLOYMENT` (add here) | `R/global.R` | after line 1 |
| `load_group_config(client_id=NULL)` | `R/persistence.R` | ~1324 |
| `save_sap_snapshot` / SAP keys | `R/persistence.R` | ~959 |
| SAP snapshot keys | `R/global.R` | `S3_KEYS_CRITICAL` (~line 119) |
| Context switcher modal | `R/tiers_module.R` | ~471–530 |
| GRUPO tab visibility observer | `app.R` | ~1174–1213 |

---

## 7. Order of Work

1. **Read** `app.R` lines 1659–1727 (context-switch observer) to understand the exact structure before adding to it
2. **Add** `load_group_config` reload (3.1) — one line
3. **Add** `.load_sap_snapshot_for_client` helper and the SAP reload block (3.2)
4. **Update** `navbar_user_badge` (3.3) — replace the existing renderUI
5. **Run** the app locally with `CLIENT_ID=networks` as Mouse — jump to any context, verify the group name and calendar update
6. **Add** `IS_ADMIN_DEPLOYMENT` to `global.R` (4.1)
7. **Update** default tab selection in `app.R` UI section (4.2)
8. **Add** financial tab show/hide observer (4.3)
9. **Run** locally with `CLIENT_ID=hd-admin` — verify admin experience and client jump
10. **Run** `source("tests/_run_saas.R")` — confirm 168/168 still green
11. **STOP** — do not deploy to ShinyApps.io yet. Ask the user for the deployment name before proceeding.

---

## 8. Design Decisions Locked by the User

- **Permissions follow the user.** Principal/hopdesk tier and all permissions stay unchanged across context switches. Staff members cannot be locked out of Grupo by jumping to a client.
- **hd-admin is an internal tool** — not visible to end clients. Name TBD (user has a candidate list).
- **SAP calendar IS needed in jumped contexts** — staff must be able to see the client's calendar when servicing them. Load from the client's S3 snapshot.
- **Same rsconnect account** for all deployments.
- **One IAM key, one bucket** — no per-client credentials for now.

---

*Authored by Claude Sonnet 4.6 — 2026-06-24*  
*Covers the two workstreams remaining after Claudio 2 completed Stages 0–7.*
