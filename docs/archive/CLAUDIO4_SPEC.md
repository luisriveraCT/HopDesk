# CLAUDIO4_SPEC — HopDesk Stage 4: True Multi-Tenancy

**Audience:** Claudio 4 (Claude Code). You have no memory of prior sessions.
**Working directory:** `C:\Users\luisr\Antiguedad_App\`
**Read this document completely before touching code.**

---

## 0. What You're Working On

HopDesk is an R Shiny SaaS financial operations platform. All state lives in AWS S3. No SQL. Auth via `shinymanager`. One shared S3 bucket, one IAM key. Data is isolated by S3 prefix: `<client_id>/key.rds` (e.g. `networks/movimientos.rds`, `yeti/movimientos.rds`).

The goal of this spec is **Stage 4: true multi-tenancy.** Read the code — it will show you how sophisticated the machinery already is. Your job is to close two specific gaps, not rewrite anything.

---

## 1. The Problem

Currently `CLIENT_ID` is a process-level env var. One deployment serves one client. Adding Yeti Inc means a second server. This doesn't scale.

The fix: one shared deployment (`CLIENT_ID=hd-admin` in `.Renviron`), where each user's S3 context is determined at login from their user record, not the env var.

**The machinery to do this already exists.** Read it before you write a line:

- `app.R` → `active_client_id` (reactiveVal, ~line 942) — the S3 prefix the session reads from
- `app.R` → `observeEvent(active_client_id(), ...)` (~line 1752, priority -3) — loads ALL financial data from any client prefix. Already works perfectly.
- `app.R` → `current_user_info()` (~line 667) — already reads `client_id` and `allowed_clients` from `res_auth` (shinymanager returns these as credential row fields)
- `R/auth.R` → `.normalize_credentials()` — already includes `client_id` and `allowed_clients` in the credential row returned to shinymanager
- `R/auth.R` → `auth_load_usuarios()` — already backfills `client_id` and normalizes `allowed_clients = "[]"` for all existing records
- `R/tiers_module.R` → context switcher (`btn_switch_ctx`) — already filters clients by `allowed_clients` for hopdesk staff, shows all to principal
- `R/tiers_module.R` → user edit modal — already has checkboxes for `allowed_clients`, already saves them
- `R/persistence.R` → `.s3_read_with(key, client_id)` — reads from any prefix, bypasses env var

**The permission UI for the principal (granting jump access to staff) is already built and working.**

---

## 2. What's Missing — Two Gaps Only

### Gap 1: Login hook (critical)

After login, `active_client_id` stays `NULL`. The context-switch observer ignores `NULL` (Shiny's `ignoreNULL = TRUE` default). So every user, regardless of their `client_id` in the record, falls back to reading from the env var prefix (`hd-admin`). Client users on a shared deployment see empty data.

**The fix:** one `observeEvent` in `app.R` that fires once on login and sets `active_client_id` from the authenticated user's `client_id`:

```r
observeEvent(current_user_info(), {
  info <- current_user_info()
  req(!is.null(info$user) && nzchar(info$user) && info$user != "unknown")
  cid <- info$client_id
  if (!is.null(cid) && nzchar(cid)) active_client_id(cid)
}, once = TRUE, ignoreNULL = TRUE)
```

Place it near the other `observeEvent(current_user_info(), ...)` blocks (~line 696–734 in `app.R`). Staff whose `client_id = "hd-admin"` will fire the context-switch observer with `effective_cid == env_cid`, which reloads native data — correct behavior. Client users will load from their prefix.

### Gap 2: Credential loading for shared deployment (critical)

`R/auth.R` → `.load_or_init_credentials()` reads `<cid>/usuarios.rds` as primary credentials. When `cid = "hd-admin"`, it only loads hd-admin users (staff). It skips client folders because the staff overlay block (`if (cid != "hd-admin")`) is not entered. Client users (e.g. `yeti/usuarios.rds`) are never loaded → Yeti users can't log in on the shared deployment.

**The fix:** when `cid == "hd-admin"`, after loading staff credentials, additionally scan the client registry and load each active client's `<client_id>/usuarios.rds`, excluding `tier %in% c("principal","hopdesk")` to avoid duplicating staff. Merge into the final credential data frame.

The client registry is at `hd-admin/client_registry.rds` — read with `read_client_registry()` from `R/persistence.R`. Each row has `client_id` and `status`. Load only `status == "active"` clients.

```r
# After loading staff_creds, inside .load_or_init_credentials():
if (cid == "hd-admin") {
  registry <- tryCatch(read_client_registry(), error = function(e) NULL)
  if (!is.null(registry) && nrow(registry)) {
    active_clients <- registry[registry$status == "active", , drop = FALSE]
    client_creds_list <- lapply(active_clients$client_id, function(ccid) {
      raw_c <- tryCatch(
        aws.s3::s3readRDS(object = paste0(ccid, "/usuarios.rds"), bucket = .s3_bucket()),
        error = function(e) NULL
      )
      if (is.null(raw_c) || !nrow(raw_c)) return(NULL)
      # Exclude staff tiers — they're already in hd-admin/usuarios.rds
      raw_c <- raw_c[!raw_c$tier %in% c("principal", "hopdesk"), , drop = FALSE]
      if (!nrow(raw_c)) return(NULL)
      .normalize_credentials(raw_c, ccid)
    })
    client_creds_list <- Filter(Negate(is.null), client_creds_list)
    if (length(client_creds_list))
      merged <- do.call(rbind, c(list(merged), client_creds_list))
  }
}
```

Place this after the existing `merged <- do.call(rbind, ...)` line in `.load_or_init_credentials()`.

### Gap 3: Schema (minor)

`R/persistence.R` → `.schema_usuarios_init()` is missing the `allowed_clients` column. Add `allowed_clients = character()` to it. The `auth.R` layer already handles missing columns via defaults, so this is cosmetic, but the schema function should be complete.

---

## 3. Non-Negotiable Rules

Carry forward from prior specs — **all of these still apply:**

1. **DO NOT split any module file.** Every `.R` in `R/` stays as-is. One file per module.
2. **168/168 tests must pass** before and after every change. Run: `source("tests/_run_saas.R")`.
3. **No dev account.** The test suite creates its own credential fixtures.
4. **`hd-admin/` paths bypass `.s3_key()`.** Never use `.s3_key()` for hd-admin prefix reads — use direct `aws.s3::s3readRDS(object = "hd-admin/...", bucket = .s3_bucket())`.
5. **`send_email()` must never throw.** It wraps in tryCatch.
6. **Principal bypasses all access restrictions.** `is_principal()` = full access, no checks.
7. **User IDs (UUID) never appear in rendered UI.**
8. **The context-switch observer (app.R ~line 1752) is not to be modified.** It works.
9. **Do NOT touch the permission UI in tiers_module.R.** The checkboxes and save logic are already correct.

---

## 4. Verification

After implementing both gaps, verify end-to-end:

1. Run `source("tests/_run_saas.R")` — 168/168 must pass
2. In the app, log in as Mouse (principal, `client_id = "hd-admin"`): should see hd-admin native view, context switcher shows all clients
3. Log in as a Networks user: should see Networks data immediately (context-switch fires on login), no context switcher visible
4. Log in as a hopdesk staff member with `allowed_clients = '["networks"]'`: context switcher shows only Networks
5. Attempt to jump to a client not in `allowed_clients`: should be rejected (the switcher won't show it — the `observeEvent(input$btn_switch_ctx)` block validates `allowed_clients` before accepting the selection)

---

## 5. Key File Index

| File | What to touch |
|------|---------------|
| `app.R` | Add login hook (Gap 1) |
| `R/auth.R` | Extend `.load_or_init_credentials()` (Gap 2) |
| `R/persistence.R` | Add `allowed_clients` to `.schema_usuarios_init()` (Gap 3) |
| `R/tiers_module.R` | **DO NOT TOUCH** — permission UI already complete |
| `tests/_run_saas.R` | Run only — do not modify |

---

## 6. Architecture Intent

HopDesk aims to serve 100,000 clients from one deployment. The S3 prefix model (`<client_id>/`) already gives true data isolation at zero marginal cost per client — no new IAM policy, no new deployment, no new code path. The session is the client. The login record is the key. The context-switch observer is the door.

Staff (principal + hopdesk) always log into hd-admin, then jump into client contexts explicitly. Client users (finance, analysis) always see only their own prefix — they have no context switcher and `active_client_id` is set at login and never changes for them.

The permission hierarchy:

```
Principal
  └─ jumps anywhere, grants other staff access to specific clients
  └─ manages all user accounts, all client registrations

HopDesk staff
  └─ jumps only to clients in their allowed_clients list
  └─ principal controls this list via Grupo → Usuarios → Edit → "Contextos permitidos"

Finance / Analysis (client users)
  └─ see only their client's data
  └─ no context switcher
  └─ active_client_id is locked to their client_id at login
```

*Authored by Claude Sonnet 4.6 — 2026-06-25*
*Claudio 1–3 built the SaaS foundation. Stage 4 (this spec) closes the multi-tenancy loop.*
