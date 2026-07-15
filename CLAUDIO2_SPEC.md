# CLAUDIO2_SPEC.md
## HopDesk SaaS — Handoff Spec for Claudio 2

**For:** Claudio 2 (Claude Code agent — you have no memory of prior sessions)  
**Access:** Full read/write to `C:\Users\luisr\Antiguedad_App\`  
**Read this entire document before touching code.**  
**Reference doc:** `CLAUDIO_SPEC.md` (original full architecture spec — read it for design decisions)

---

## 0. What You're Walking Into

This is an R Shiny financial operations SaaS platform. All persistent state is in AWS S3. Auth via `shinymanager`. No database — every data structure is an `.rds` file in S3.

Claudio 1 implemented a significant portion of the SaaS foundation but went off-script and broke the Settings module by splitting it into files (`R/settings/*.R`) without understanding Shiny's environment loading model. **Those bugs have already been fixed by the supervising Claude before you started** — do not revisit them. Specifically, do NOT split any more module files (bancos_module.R, tiers_module.R, etc.) — that work is outside scope and caused the breakage.

---

## 1. Current Codebase State

### Environment — ALREADY SIMPLIFIED

`persistence.R` already uses `S3_BUCKET` (not `NETWORKS_S3_BUCKET`). `global.R` `.check_env_vars()` validates `S3_BUCKET` + standard `AWS_*`. The `.Renviron` file on the developer's machine should already reflect this. Do NOT change persistence.R env var logic.

### What Works Today

- Full financial app for `CLIENT_ID=networks` deployment (Networks Group users)
- `auth.R` loads credentials from BOTH `<client_id>/usuarios.rds` AND `hd-admin/usuarios.rds` (staff overlay for principal + hopdesk tier)
- `principal` tier fully defined in `auth_resolve_perms()` (`R/auth.R:195–285`) with locked permissions
- `is_principal` reactive in `tiers_module.R`
- Locked permission UI (can_approve_clients + can_manage_hopdesk_perms) rendered as disabled with lock icon for principal accounts
- `email_service.R` implemented: `send_email()`, `read_email_queue()`, `write_email_queue()`; fails silently, queues to `hd-admin/email_queue.rds`
- Emergency lock persistence functions: `read_emergency_lock()`, `write_emergency_lock()` in `persistence.R:99–140`
- Username index persistence functions: `check_username_available()`, `register_username()`, `unregister_username()` in `persistence.R:146–230`
- Client registry persistence: `read_client_registry()`, `write_client_registry()`, `read_client_requests()`, `write_client_requests()`, `read_hd_notifications()`, `write_hd_notifications()` in `persistence.R:1483–1680`
- `initialize_client_folder()` in `persistence.R:1601` — creates empty schema files for a new client
- `requires_password_change` field exists in `auth.R` schema backfill (`auth.R:147`)
- `email` field exists in `auth.R` schema backfill
- Settings module split into `R/settings/*.R` — **fixed: sources use `local=TRUE` in `global.R`**
- Comprehensive test suite in `tests/` including `tests/test_saas_*.R` files

### Test Framework

Run tests from project root:
```r
source("tests/_run_saas.R")     # SaaS-specific tests (perms, invites, isolation, etc.)
source("tests/_run_s1.R")       # Stage 1 tests
source("tests/_run_forecasting.R")  # Forecasting tests (58/58 passing)
```

Always run `_run_saas.R` after any change to auth.R, persistence.R, or tiers_module.R. These tests use an in-memory S3 mock — no live credentials needed.

### What Is NOT Working / NOT Built

- **`dev` login**: Expected — auto-seeding removed. The `dev` account no longer exists. This is correct.
- **Context switcher**: `active_client_id` is still cosmetic. Switching clients in the modal does nothing to S3 data routing. This is Stage 4 — do not attempt it before Stages 0–3 are complete.
- **Emergency lock UI**: Functions exist in persistence.R, not yet wired to a UI in tiers_module.R
- **Username index NOT wired**: `check_username_available()` / `unregister_username()` are in persistence.R but NOT called from tiers_module.R user creation/deletion flows
- **Client management UI**: Schemas and persistence functions exist. UI panels (Client registry, pending requests, approval flow) do NOT exist in tiers_module.R
- **User limit enforcement**: No UI for warnings, "Solicitar usuarios" button, or deactivation panel
- **Invite system**: No UI, no token interception in app.R, no password-set screen
- **requires_password_change gate**: Field exists but app.R does NOT intercept first logins
- **Seed scripts**: `scripts/seed_hd_admin.R` does NOT exist yet — Mouse and Bunny are NOT in `hd-admin/`
- **Docker files**: `Dockerfile` and `docker-compose.yml` do NOT exist
- **Contacts database**: `.schema_contacts()` referenced in `initialize_client_folder()` but schema may not be fully defined
- **Audit log redesign**: `app_audit.R` still uses the original pattern (no `s3_key` column, no chunking)

---

## 2. Strict Rules — Read Before Starting

1. **DO NOT split any module file.** `bancos_module.R`, `tiers_module.R`, `app.R`, etc. must stay as single files. Claudio 1 split `settings_module.R` and broke the app. The `R/settings/` split is already done and fixed — don't touch it.

2. **DO NOT modify tests without running them first.** Run `_run_saas.R` before and after any change to verify nothing regressed.

3. **Stage order matters.** Complete stages in order. Do not start Stage 3 UI before Stage 2 wiring is done.

4. **`dev` account is gone by design.** If you see `dev / Antiguedad2026!` in any bootstrap routine still remaining, remove it. It is NOT a bug that dev can't log in.

5. **Never throw from `send_email()`.** That function must always return silently.

6. **`hd-admin/` paths bypass `.s3_key()`.** Functions that read/write `hd-admin/` must use direct `aws.s3::s3readRDS` / `aws.s3::s3saveRDS` with the full `hd-admin/xxxx.rds` key — not the `.s3_key()` helper which prepends the `CLIENT_ID` prefix.

7. **User IDs (UUID) must never appear in rendered UI.** The `id` column must be excluded from every `DT::datatable()` call. Audit all modules.

---

## 3. Remaining Work — Ordered by Stage

### Stage 0 Remaining: Foundation Cleanup

- [ ] **0.1 Docker files** — Add `Dockerfile` (standard `rocker/shiny` base, `renv::restore()`) and `docker-compose.yml` (two-service example: `networks-client` + `hopdesk-admin`, each with `env_file`). Include heavy comments explaining this is a blueprint for future self-hosting. See `CLAUDIO_SPEC.md` Section 9.
- [ ] **0.2 Seed `hd-admin/` folder** — Write `scripts/seed_hd_admin.R`:
  - Creates `hd-admin/usuarios.rds` with two accounts: Mouse (principal, `requires_password_change=FALSE`) and Bunny (hopdesk, `requires_password_change=TRUE`)
  - Mouse: `username="mouse"`, `display_name="Luis Rivera"`, `tier="principal"`, `allowed_clients='["networks","hopdesk"]'`, `can_jump_clients=TRUE`
  - Bunny: `username="bunny"`, `display_name="Ana Fernanda Nemer"`, `password_hash="PBunny129!"`, `tier="hopdesk"`, `allowed_clients='["networks","hopdesk"]'`, `can_jump_clients=TRUE`, `can_manage_invites=TRUE`
  - Removes Mouse from `networks/usuarios.rds` (he moves to `hd-admin/` exclusively)
  - Creates empty schema files for `hopdesk/` folder: `hopdesk/usuarios.rds` (empty), `hopdesk/empresas.rds` (empty), `hopdesk/grupos.rds` (empty), `hopdesk/app_audit.rds` (empty)
  - Registers Mouse and Bunny in `hd-admin/username_index.rds`
  - Writes empty `hd-admin/emergency_lock.rds` (zero rows)
  - Sources only `R/persistence.R` + inline `S3_KEYS` — no Shiny deps
- [ ] **0.3 Update `scripts/recover_mouse.R`** — Uses `s3_init()` which may still expect the old env var pattern. Verify it works with `S3_BUCKET`.
- [ ] **0.4 Verify `.Renviron`** — Confirm it contains `S3_BUCKET` (not `NETWORKS_S3_BUCKET`), and standard `AWS_*` vars. Add `RESEND_API_KEY=sandbox` and `RESEND_FROM_EMAIL=noreply@hopdesk.com` as placeholders.
- [ ] **0.5 `s3_init()` check** — Read `R/persistence.R:42–80`. Confirm `s3_init()` initializes from `S3_BUCKET` + `AWS_*`. If it still reads any `{UPPER_CLIENT_ID}_*` variables, simplify it.

### Stage 1 Remaining: Permission System + Emergency Security

- [ ] **1.1 Emergency lock check in `auth.R`** — After the standard credential check in `.load_or_init_credentials()`, read `hd-admin/emergency_lock.rds` (use `read_emergency_lock()`, cached 60s). If the authenticating username appears in the lock file, exclude that row from the returned credentials data.frame so `shinymanager` rejects the login.
- [ ] **1.2 Session termination observer in `app.R`** — After shinymanager auth, add an observer on `current_user_info()` that calls `read_emergency_lock()` and calls `session$reload()` if the current user is locked. Runs once per session initialization.
- [ ] **1.3 Emergency lock UI in `tiers_module.R`** — In the "Seguridad" tab (already scaffolded at `tiers_module.R:153`): add a DT table of locked accounts (from `read_emergency_lock()`), a "Lock account" button that opens a modal (username + reason), and an "Unlock" button per row. Only visible to `is_principal()`.
- [ ] **1.4 `requires_password_change` gate in `app.R`** — After shinymanager authentication, check the authenticated user's `requires_password_change` field (from `credentials()$info`). If TRUE, show only a password-change UI (same design as the invite password-set screen — minimal, no nav modules visible). On confirmed password save: update `requires_password_change = FALSE` in `<client_id>/usuarios.rds`, save, reload session.
- [ ] **1.5 User ID hiding audit** — Grep all `renderDataTable` / `DT::datatable` calls in all module files. Ensure `id` column is excluded from EVERY rendered table. The `account_code` column is already fixed in `tiers_module.R` — audit the rest.
- [ ] **1.6 `scripts/emergency_rewind.R`** — Standalone script (runs locally). See `CLAUDIO_SPEC.md` Section 2.3 for full spec.
- [ ] **Stage 1 tests** — Run `_run_saas.R`. Add tests to `tests/test_saas_perms.R` for emergency lock: locked account rejects login, active session terminates.

### Stage 2 Remaining: Global Username Registry

- [ ] **2.1 Wire `check_username_available()` into user creation** — In `tiers_module.R` `observeEvent(input$btn_save_new_user, ...)` (around line 544): call `check_username_available(username)` BEFORE writing to `usuarios.rds`. If unavailable, show error notification: "Este nombre de usuario ya está registrado en el sistema."
- [ ] **2.2 Wire `register_username()` into user creation** — After successful write to `usuarios.rds`, call `register_username(username, home_folder=CLIENT_ID, account_code=new_row$account_code)`.
- [ ] **2.3 Wire `unregister_username()` into user deletion** — Find the delete observer in `tiers_module.R` (around line 640). After successful deletion from `usuarios.rds`, call `unregister_username(username)`.
- [ ] **Stage 2 tests** — `tests/test_saas_isolation.R` likely has isolation tests. Verify username uniqueness enforcement.

### Stage 3: Client Management UI

All persistence functions already exist. This stage is UI wiring in `tiers_module.R`.

- [ ] **3.1 "Clientes" tab UI (principal/hopdesk)** — The tab is scaffolded at `tiers_module.R:160`. Build:
  - Client list table: display_name, current_users/max_users, status badge. Amber row at 2 remaining, red row at full. Read from `read_client_registry()`.
  - "New Client" button → modal: proposed slug, display name, contact email, max_users, notes → submit writes to `write_client_requests()` with status="pending", logs action
  - "Edit" button per client row → opens client editor modal: display_name, contact_email, max_users (with confirmation flow including deactivation panel when lowering below active count — see CLAUDIO_SPEC.md Section 4.4 for full spec)
  - "Review" panel (principal only): table of pending requests with approve/reject action
- [ ] **3.2 On client approval** — Call `initialize_client_folder(client_id)`, update request status, update registry, log to `hd-admin/app_audit.rds`.
- [ ] **3.3 User limit notifications** — When `max_users - current_users <= 2`: call `send_email()` to `contact_email` AND to all hd-admin staff with `can_manage_invites`. Write to `hd-admin/notifications.rds`. Fire from the user-create path in tiers_module (after incrementing `current_users`).
- [ ] **3.4 "Notificaciones" tab** (principal/hopdesk) — Scaffolded at `tiers_module.R:174`. Show unread notifications from `read_hd_notifications()`. Mark as read on open.
- [ ] **3.5 Client-side limit UI** — In `tiers_module.R` "Nueva cuenta" button area: check `current_users >= max_users` from `client_registry.rds`. If at limit: replace "Nueva cuenta" with "Solicitar usuarios" (one-press, writes `type="user_request"` notification, transitions to "Solicitud enviada" state).
- [ ] **3.6 "Contacts" panel in client editor** — CRUD for `<client_id>/contacts.rds`. Read `CLAUDIO_SPEC.md` Section 10.
- [ ] **Stage 3 tests** — `tests/test_saas_notifications.R` and `tests/test_saas_limit_change.R` likely exist. Run and ensure they pass.

### Stage 4: Cross-Client Staff Access

This is the largest remaining stage. Read `CLAUDIO_SPEC.md` Section 5 completely before starting.

- [ ] **4.1 `allowed_clients` multi-select in tiers UI** — When editing a hopdesk/principal user (principal-tier viewer only), show multi-select of all clients from `read_client_registry()`. Only shown when `can_jump_clients=TRUE`. Write as JSON field on the user record.
- [ ] **4.2 Client selector UI in hd-admin deployment** — When `CLIENT_ID=hd-admin` AND user has `can_jump_clients`, show a client selector in the nav (or tiers module). Selector built ONLY from the user's `allowed_clients` field — never the full registry. "HopDesk" always first.
- [ ] **4.3 `session_client_id()` reactive** — Returns `active_client_id() %||% tolower(Sys.getenv("CLIENT_ID"))`. Wire this into ALL `load_*()` functions that use `.s3_key()` for their reactive calls. This is the actual S3 prefix override that makes the context switch work. See `CLAUDIO_SPEC.md` Section 5.3 for the full implementation note.
- [ ] **4.4 Dual audit logging** — All reads/writes while in jump context log to BOTH `hd-admin/app_audit.rds` AND `<client_id>/app_audit.rds`. See CLAUDIO_SPEC.md Section 5.4.
- [ ] **Stage 4 tests** — `tests/test_saas_isolation.R` should cover this. Verify that jumping to a client doesn't persist data to the wrong S3 prefix.

### Stage 5: Invites and Email

- [ ] **5.1 `hd-admin/pending_invites.rds`** — Add schema (`.schema_pending_invites()`) and read/write functions to `persistence.R`. See `CLAUDIO_SPEC.md` Section 6.2 for schema.
- [ ] **5.2 Invite generation UI** — In tiers_module "Notificaciones" or dedicated "Invitaciones" panel (with `can_manage_invites`): form for email, tier, client → call `send_email()` with invite link + write to `pending_invites.rds`.
- [ ] **5.3 Token interception in `app.R`** — Early in server(), read `session$clientData$url_search`. If `invite=<token>` present: validate token (not expired, not used, not killed) against `pending_invites.rds` → show password-set UI only, no other modules.
- [ ] **5.4 Password-set UI** — display_name, new password, confirm password. Validation: ≥6 characters, at least one number, at least one symbol. On confirm: save user → mark token used → register in username_index → log.
- [ ] **5.5 Invite management panel** — Table of invites per client: status (Pending/Used/Expired/Killed), Kill button, Resend button.
- [ ] **Stage 5 tests** — `tests/test_saas_invites.R` likely exists. Run it.

### Stage 6: Audit Infrastructure

- [ ] **6.1 Add `s3_key` column to audit log** — In `R/app_audit.R`, add `s3_key` to the schema and to the `log_action()` signature. Populate it on every write operation. This is the rewind correlation column.
- [ ] **6.2 Chunked rotation** — When `app_audit.rds` exceeds 10,000 rows, rotate: rename to `app_audit_001.rds`, `app_audit_002.rds`, etc. Add `read_audit_log(client_id, since=NULL)` that reads all chunks and rbinds.
- [ ] **6.3 Wire all new actions** — Ensure every SaaS action added in Stages 1–5 calls `log_action()` with `s3_key` populated.
- [ ] **6.4 `scripts/emergency_rewind.R`** — Uses `read_audit_log()` to find compromised writes by key. See CLAUDIO_SPEC.md Section 2.3 for full spec.

### Stage 7: IAM Hardening

- [ ] **7.1 Generate IAM policy JSON** — Prefix-level `ListBucket` restriction. See `CLAUDIO_SPEC.md` Section 8.

---

## 4. File Locations and Key Functions

| What | Where |
|------|-------|
| S3 isolation | `R/persistence.R` — `.s3_key()`, `.s3_bucket()`, `s3_init()` |
| Tier defaults + locked perms | `R/auth.R:195–285` — `auth_resolve_perms()` |
| Credential loading (SaaS-aware) | `R/auth.R:35–80` — `.load_or_init_credentials()` |
| Emergency lock persistence | `R/persistence.R:99–140` |
| Username index | `R/persistence.R:146–230` |
| Client registry / requests | `R/persistence.R:1483–1680` |
| Folder initialization | `R/persistence.R:1601` — `initialize_client_folder()` |
| Email service | `R/email_service.R` — `send_email()`, queue helpers |
| User management UI | `R/tiers_module.R` |
| Settings module (split, fixed) | `R/settings/*.R` — sourced with `local=TRUE` in `global.R:58–67` |
| Test runner (SaaS) | `tests/_run_saas.R` |
| Test runner (all SaaS tests) | `tests/test_saas_*.R` |

---

## 5. S3 Folder Structure (Current + Target)

```
antiguedad-rds-prod/
  networks/          ← Networks Group client (all financial data, original users)
  hd-admin/          ← HopDesk admin (NOT yet seeded — seed script needs to run)
  hopdesk/           ← HopDesk company client (empty schemas — no active deployment)
```

After `scripts/seed_hd_admin.R` runs:
- `hd-admin/usuarios.rds` = Mouse (principal) + Bunny (hopdesk)
- `hd-admin/username_index.rds` = all known usernames
- `hd-admin/emergency_lock.rds` = empty schema
- `networks/usuarios.rds` = all Networks users MINUS Mouse (moved to hd-admin)
- `hopdesk/*` = empty schemas

---

## 6. What Claudio 1 Did That Was Outside Scope (Do Not Repeat)

- Splitting `settings_module.R` into `R/settings/*.R` — was not in the spec, broke things. Files are now fixed and working. Do NOT split `bancos_module.R`, `tiers_module.R`, `app.R`, or any other large module.
- Planning to split `bancos_module.R` next — explicitly do NOT do this.
- The settings split IS done and fixed. Leave it alone.

---

## 7. Immediately Before You Start

1. **Verify the settings fix is working** — start the app, log in as Mouse, navigate to Settings → Catálogo de Proveedores. It should load without errors. If it crashes, the `local=TRUE` fix in `global.R:58–67` is the relevant code.

2. **Run the SaaS test suite**: `source("tests/_run_saas.R")`. All tests should pass (or at least not regress from what they were before you start). Document any failing tests before making changes.

3. **Read `CLAUDIO_SPEC.md`** for the full architecture decisions and design rationale behind every feature. This document tells you WHAT to build; CLAUDIO_SPEC.md tells you WHY and what the full data schemas look like.

4. **Start with Stage 0 remaining items** (Docker files + seed script). These are mechanical and low-risk. Confirm with the user that the seed script output looks correct before running it on live S3 data.

---

*Authored by Claude Sonnet 4.6 — 2026-06-24*  
*Claudio 1 Executive Summary incorporated and diagnosed above.*
