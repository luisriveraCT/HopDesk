# Antigüedad App — Authentication Knowledge Base

> **Status:** Planning phase. No code written yet.
> **Last updated:** March 2026
> **Scope:** Multi-tenant SaaS authentication with 4 permission tiers.

---

## Option Comparison — Authentication Approaches

### Free Options

#### Option A — `shinymanager` (Recommended for Phase 1)
- **What it is:** R package that wraps any Shiny app with a login screen. Credentials stored in an encrypted SQLite database or a simple data frame.
- **How it works:** Wraps `ui` and `server` with `secure_app()` / `secure_server()`. One line each.
- **Pros:** Zero infrastructure, works entirely within R, no external service, credential management UI included, supports hashed passwords (bcrypt), role column built-in.
- **Cons:** Single-process — all sessions share one R process. Not suitable for high concurrency (>20 simultaneous users). SQLite file must be on a shared volume if running multiple workers.
- **Best for:** Phase 1 — single deployment, Networks Group internal use, up to ~15 concurrent users.
- **Cost:** Free forever.

#### Option B — `firebase` R package + Google Firebase Auth (Free tier)
- **What it is:** Firebase Authentication via the `firebase` R Shiny package. Google handles passwords, sessions, and 2FA.
- **How it works:** JS SDK in the browser talks to Firebase; R receives a verified user token via `input$user`.
- **Pros:** Industry-grade security, Google manages passwords and tokens, free up to 10,000 users/month, supports email/password + OAuth (Google, Microsoft). Token-based — stateless on the R side.
- **Cons:** Requires a Firebase project setup (15 min), internet dependency for auth, more complex role management (roles stored separately in S3 or Firestore).
- **Best for:** Phase 2 — when the app goes multi-tenant SaaS. Scales to thousands of users.
- **Cost:** Free up to 10k MAU. Paid above that (~$0.0055/MAU).

#### Option C — Roll-your-own with `session$userData` + S3 credentials
- **What it is:** Custom login screen in Shiny, passwords hashed with `bcrypt`, user records in S3.
- **Pros:** Full control, no external dependencies, fits exactly into existing S3 persistence pattern.
- **Cons:** You own security — password reset, session expiry, brute force protection all manual. High maintenance.
- **Best for:** Never — too much risk for auth infrastructure.

### Paid Options

#### Option D — Posit Connect (formerly RStudio Connect)
- **What it is:** Enterprise deployment platform for Shiny apps. Has built-in user management, LDAP/SSO integration.
- **Pros:** Professional, supports SAML/OAuth/LDAP, per-app access control, audit logs.
- **Cons:** $15k+/year. Overkill for current scale.
- **Best for:** Large enterprise (50+ internal users, compliance requirements).

#### Option E — Auth0 (Freemium)
- **What it is:** Dedicated identity platform. Free up to 7,500 MAU, then ~$23/month.
- **How it works:** Similar to Firebase but more enterprise-focused. Has R/Shiny integration via `httr` + OAuth2 flow.
- **Pros:** Universal login, MFA, anomaly detection, extensive audit logs, SAML for enterprise SSO.
- **Cons:** Requires more setup than Firebase, slightly more complex R integration.
- **Best for:** Phase 2 if multi-company clients need SSO with their own identity providers.

---

## Recommended Path

```
Phase 1 (now):     shinymanager  — internal Networks Group use
Phase 2 (SaaS):    Firebase Auth — multi-tenant, scales to thousands
```

Start with `shinymanager` because:
1. Zero external dependencies — works on the existing Windows machine today
2. One-day implementation
3. Migration to Firebase later is clean — roles/permissions logic is identical, only the credential store changes

---

## Permission Tier Design

### Tier A — Dev
- Full access to everything
- Future: analytics dashboard, audit trail of all user actions, deleted/modified content history
- Can manage all user accounts and permissions from within the app
- Can see system diagnostics (S3 keys, SAP connection status, performance logs)
- Only 1-2 accounts ever exist at this tier

### Tier B — Admin
- Full access to all app features (CxC, CxP, Agenda de Hoy, Bancos, Configuración, Proveedores)
- No system diagnostics, no user management
- Can assign/modify Finance and Analysis tier permissions
- One per client company group

### Tier C — Finance
- Access to CxC, CxP, Agenda de Hoy, Bancos
- Action buttons restricted based on Admin configuration per-user:
  - `can_confirm_payments` — Confirmar pago/cobro in Agenda de Hoy
  - `can_move_invoices` — Mover/Restaurar dates in calendar
  - `can_delete_invoices` — Eliminar from calendar
  - `can_import_bank` — Importar TXT in Bancos
  - `can_vincular` — Vincular/Conciliar in Bancos
- Configuración tab: read-only (can see settings, cannot change)

### Tier D — Analysis
- Read-only by default
- Admin defines which modules are visible per account:
  - `can_see_cxc` — Cobros calendar
  - `can_see_cxp` — Pagos calendar
  - `can_see_agenda` — Agenda de Hoy (view only, no actions)
  - `can_see_bancos` — Bancos Libro de Banco (view only)
- No action buttons visible
- May be password-only (no username) for simple sharing — implemented as a named shared account

---

## Data Schema — User Records

Stored in S3 as `usuarios.rds` (new key, under `S3_KEYS` deferred list):

```r
.schema_usuarios <- function() tibble::tibble(
  id           = character(),   # UUID
  username     = character(),   # login name (email or short handle)
  password_hash = character(),  # bcrypt hash — NEVER store plaintext
  display_name = character(),   # shown in UI
  tier         = character(),   # "dev" | "admin" | "finance" | "analysis"
  client_id    = character(),   # which CLIENT_ID this user belongs to
  permisos     = character(),   # JSON string of permission flags (Finance/Analysis)
  activo       = logical(),     # can be deactivated without deleting
  created_at   = character(),
  last_login   = character()
)
```

`permisos` JSON structure for Finance tier:
```json
{
  "can_confirm_payments": true,
  "can_move_invoices": true,
  "can_delete_invoices": false,
  "can_import_bank": true,
  "can_vincular": false
}
```

`permisos` JSON structure for Analysis tier:
```json
{
  "can_see_cxc": true,
  "can_see_cxp": false,
  "can_see_agenda": true,
  "can_see_bancos": false
}
```

---

## App Architecture — How Auth Wraps the Existing App

### Login gate
The entire `ui` is wrapped. Unauthenticated users see only the login screen — no Shiny UI loads, no S3 reads happen until login succeeds.

```r
# app.R — conceptual structure after auth
ui <- auth_ui_wrapper(
  page_navbar(...)  # existing UI, unchanged
)

server <- function(input, output, session) {
  user <- auth_server_wrapper(input, output, session)
  # user() returns list(username, tier, permisos) or NULL

  req(user())  # all existing server code runs only after login
  # ... existing server code unchanged below here ...
}
```

### Permission enforcement
A `perms` reactive derived from `user()` is added to `shared`:
```r
shared$user     = user          # reactive returning current user record
shared$tier     = reactive(user()$tier)
shared$perms    = reactive(jsonlite::fromJSON(user()$permisos %||% "{}"))
```

UI elements are shown/hidden via `conditionalPanel` or `renderUI` checks against `shared$tier` and `shared$perms`. **Server-side enforcement** must also exist — hiding a button is not enough, the `observeEvent` handler must also check permissions.

### Nav panel visibility (Analysis tier)
```r
# In ui — conditionalPanel wraps each nav_panel
# Analysis users only see their allowed modules
```

Since `bslib::page_navbar` doesn't support dynamic panel visibility natively, Analysis tier nav panels are rendered via `uiOutput("main_nav")` which returns the full navbar or a restricted version based on the authenticated tier.

---

## Implementation Steps (Sequential — confirm each before next)

### Step 1 — Install + wire `shinymanager` (no permissions yet)
- Add `library(shinymanager)` to `global.R`
- Add `usuarios.rds` to `S3_KEYS` in `global.R`
- Create `R/auth.R`: `load_usuarios()`, `save_usuarios()`, `init_usuarios()` (creates Dev account on first run)
- Wrap `ui` with `secure_app()`, wrap server with `secure_server()`
- Verify login screen appears, Dev account can log in, existing functionality unchanged
- **Do not implement permissions yet**

### Step 2 — User record schema + S3 persistence
- Implement `.schema_usuarios`, `load_usuarios`, `save_usuarios` in `persistence.R`
- Add `usuarios_db` reactiveVal to `app.R`, wire into `shared`
- Add `shared$user`, `shared$tier`, `shared$perms` reactives
- Seed first Dev account on first run if `usuarios.rds` absent
- Verify `shared$tier()` returns correct value after login

### Step 3 — Nav panel gating (Analysis tier)
- Wrap nav panels in conditional rendering based on `shared$tier` and `shared$perms`
- Analysis users see only their permitted modules
- All other tiers see everything (permissions within modules handled in Step 4)
- Verify by creating a test Analysis account with limited visibility

### Step 4 — Action button gating (Finance tier)
- Add permission checks to `observeEvent` handlers for:
  - Confirmar pago/cobro (pagar_hoy_module.R)
  - Mover/Restaurar (ledger_module.R)
  - Eliminar from calendar (ledger_module.R)
  - Importar TXT (bancos_module.R)
  - Vincular/Conciliar (bancos_module.R)
- Hide buttons in UI when permission is FALSE
- Server-side: `req(shared$perms()$can_confirm_payments)` before executing
- Configuración tab: read-only for Finance tier

### Step 5 — User management UI (Admin + Dev)
- New tab inside Configuración: "Usuarios"
- Admin can: create/edit Finance and Analysis accounts, set permisos JSON, deactivate
- Dev can: do all of the above + manage Admin accounts + see last_login
- Password reset: Admin sets a temporary password, user must change on first login
- Verify full CRUD works, permissions update immediately on next login

### Step 6 — Session management + security hardening
- Session timeout: auto-logout after 8 hours of inactivity
- Failed login attempt counter (lock after 10 attempts)
- Audit log: write to `audit_log.rds` on login, logout, permission changes
- Password policy: minimum 8 chars, bcrypt cost factor 12

---

## Files That Will Be Created/Modified

| File | Change |
|---|---|
| `R/auth.R` | New — login UI, credential verification, session management |
| `R/persistence.R` | Add `.schema_usuarios`, `load_usuarios`, `save_usuarios` |
| `global.R` | Add `library(shinymanager)`, `source("R/auth.R")`, add `usuarios` to `S3_KEYS` |
| `app.R` | Wrap UI + server with auth, add `shared$user/tier/perms`, add `usuarios_db` reactiveVal |
| `R/pagar_hoy_module.R` | Step 4 — permission checks on confirm handlers |
| `R/ledger_module.R` | Step 4 — permission checks on move/delete/stage handlers |
| `R/bancos_module.R` | Step 4 — permission checks on import/vincular handlers |
| `R/settings_module.R` | Step 5 — new Usuarios tab |

---

## Security Rules (Non-Negotiable)

**Never store plaintext passwords.** Always bcrypt hash with cost ≥ 12.

**Server-side enforcement is mandatory.** Hiding a UI button is cosmetic. Every sensitive `observeEvent` must `req(permission)` before executing.

**`usuarios.rds` is sensitive.** It contains password hashes. Ensure the S3 bucket policy restricts read access to the app's IAM role only. Never log its contents.

**`shared$tier` and `shared$perms` are read-only from the server's perspective.** They are derived from the authenticated user record at login. No client-side input should be able to change them.

**Multi-tenant isolation.** When this becomes SaaS, each client's `usuarios.rds` lives under their `CLIENT_ID/` prefix in S3. Users from one client can never access another client's data — enforced by the `CLIENT_ID` prefix on all S3 reads, which is set at process start from `.Renviron`.
