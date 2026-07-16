# Stage 3 — Per-Client ERP Credentials

Read `ARCHITECTURE.md` (§8) first. Stages 1 and 2 are merged into
`saas-checkpoint-2026-07-15` and confirmed passing (219/219) — branch this
stage off that branch, not off `stage-2-home-jump-split` directly.

Housekeeping check before this stage starts: `.Renviron` (which holds every
real secret this app uses today — AWS keys, SAP passwords, a VPN password)
was confirmed never committed to git history and is correctly gitignored.
Good — but this stage exists precisely because the *next* generation of
these secrets is about to move from `.Renviron` into S3, so the encryption
design below matters. Do not relax it because ".Renviron was fine."

## Why this stage, and what "done" looks like

Today, `R/sap_api.R` reads `SAP_{INITIALS}_URL/COMPANY/USER/PASSWORD` env
vars from the one shared `.Renviron` — real credentials for Networks
Group's 5 companies, hardcoded, plaintext, editable only by hand-editing a
file on this machine and redeploying. No other client can ever connect an
ERP. "Done" for this stage means: any client's own `dev`-tier user can add,
edit, test, and delete their own company's ERP connection through the app,
with zero hardcoded credentials left anywhere in the codebase — Networks
included.

Mouse's explicit requirements, restated so they're not lost in translation:
- Fully self-serve by the client. Hopdesk staff can also do it *for* a
  client, but only via the existing jump mechanism — never a separate
  staff-only path.
- Extensible: different ERPs need different fields entirely (this app
  will support more than SAP eventually — Oracle was named specifically).
  Don't hardcode SAP's shape as if it were universal.
- A client may need more than one credential set — Networks needs one per
  company (5 today). Design for "N credentials, each scoped to one or more
  companies," not "exactly one credential per client."
- Absolute certainty that a client's connection can only ever use that
  client's own credentials — never another client's, never Hopdesk's.
- Secrets encrypted before they reach S3. Simple for now (one master key,
  no KMS) — but leave clear comments marking the upgrade path.
- Migrate Networks' real credentials in as part of this stage, pre-filled —
  Mouse does not want to have to go find these values again.
- Built for "an IT person configures this once, then doesn't touch it for
  a year" — so favor a clear, well-labeled form and a "test connection"
  action over anything clever, and don't make the UI depend on the person
  remembering context from a year ago.

## Design

### 1. Schema: `erp_connections.rds`, one per client, at `<client_id>/erp_connections.rds`

```
id                 — UUID
client_id          — redundant with the S3 folder itself, but stored and
                     re-checked on every read/write as defense in depth —
                     see §4 (isolation guarantee) for why this matters
label              — display name, e.g. "SAP — Networks Group (NG)"
erp_type           — key into the connector registry (§2) — "sap_b1_service_layer"
                     is the only value implemented in this stage
company_initials   — JSON array of Empresas `initials` this connection
                     serves. Almost always length 1 today (SAP B1 Service
                     Layer needs one login per company) — the array shape
                     exists so a future ERP type that covers several
                     companies with one login doesn't need a schema change.
config             — JSON string, NON-secret fields only (url, database/
                     company name, ssl_verify, etc.) — shape is entirely
                     defined per erp_type by the connector registry, not by
                     this schema
secrets_encrypted  — encrypted blob (see §3) containing the erp_type's
                     secret fields (user, password, API key, whatever that
                     type defines) — NEVER written anywhere unencrypted,
                     including logs and error messages
created_at, created_by, updated_at, updated_by
last_tested_at, last_test_result   — "ok" | "error" | NA
last_test_message — human-readable result of the most recent test
active             — logical
deleted, deleted_at — soft delete, same convention as usuarios.rds
```

### 2. Connector registry: `R/erp_connector_registry.R` — the extension point

Mirrors `R/tier_registry.R`'s role from Stage 1: one place that defines what
fields an ERP type needs, so the UI form and validation are generated from
it rather than hardcoded per type.

```
ERP_CONNECTOR_REGISTRY structure per erp_type:
  label          — display name shown in the "add connection" type picker
  config_fields  — list of {name, label, type ("text"|"checkbox"), required}
                   for the NON-secret fields (rendered into `config`)
  secret_fields  — list of {name, label, type ("text"|"password")} for the
                   fields that go into `secrets_encrypted`
  test_fn        — name of the function that performs a live test given a
                   resolved (decrypted) credential + config, returns
                   list(ok = TRUE/FALSE, message = "...")
```

Implement exactly one entry, `sap_b1_service_layer`, matching today's
`SAP_{INITIALS}_*` shape 1:1: `config_fields` = url, company, ssl_verify;
`secret_fields` = user, password; `test_fn` = a thin wrapper around
whatever `R/sap_api.R` already uses to open a session (reuse, don't
reimplement, the existing SAP Service Layer login call).

**Leave a comment block directly below this entry, in this exact shape,
marking where a future ERP type gets added** — no code, no invented field
names for Oracle or anything else not scoped yet, just the shape:

```r
# ── Add a new ERP type here ─────────────────────────────────────────────
# ERP_CONNECTOR_REGISTRY[["your_new_type"]] <- list(
#   label = "...",
#   config_fields = list(...),
#   secret_fields = list(...),
#   test_fn = "your_test_function_name"
# )
# Then implement your_test_function_name() near wherever that ERP's API
# client lives (a new R/<erp>_api.R, following R/sap_api.R's shape).
```

### 3. Encryption: `R/secrets_encryption.R`

One master key, read from a new env var — `HOPDESK_SECRETS_KEY` (32
random bytes, base64-encoded), added to `.Renviron` (already gitignored,
confirmed above) and documented in this file's header comment with the
exact command to generate one (e.g. `openssl rand -base64 32`).

```
encrypt_secret(named_list) → character (base64 ciphertext)
decrypt_secret(ciphertext) → named_list
```

Use AES-256-GCM (via the `openssl` R package — check whether it's already
a dependency; if not, add it to whatever this project uses for package
management and to `global.R`'s `library()` block). GCM gives you
authenticated encryption (tamper detection) for free over CBC — use it, not
a weaker mode, even though this is "simple for now."

Add a comment block at the top of this file marking the upgrade path
explicitly, so it isn't silently forgotten:

```r
# Upgrade path (not needed yet, noted for later): move from one shared
# HOPDESK_SECRETS_KEY to a per-client key (derived or stored in AWS KMS),
# so compromising the one key doesn't expose every client's secrets at
# once. Revisit if/when a client's contract requires it, or once there are
# enough clients that blast radius becomes a real concern.
```

**Never** log a decrypted secret, include one in an error message shown to
the user, or write one into `app_audit.rds` — when this feature's actions
are audited (create/edit/test a connection), log the connection's `label`
and `erp_type` and result, never the config/secret payload.

### 4. The isolation guarantee — how to make "only this client's credentials, ever" provable, not just intended

- Every function that reads or writes `erp_connections.rds` takes an
  explicit `client_id` parameter — no silent fallback to
  `Sys.getenv("CLIENT_ID")` the way some older persistence helpers do. This
  stage's functions should be impossible to call without stating whose
  folder you mean.
- On every read, after loading `<client_id>/erp_connections.rds`, assert
  every row's own `client_id` column matches the folder you read it from —
  a defense-in-depth check that would catch a future S3-key mistake before
  it ever reaches a live ERP call. Fail loudly (stop with an error) rather
  than silently filtering, if this ever mismatches — that would mean
  something upstream is already broken and hiding it is worse.
- The function that actually opens an ERP connection (e.g. a new
  `fetch_all_companies()`-equivalent path, or the existing one once
  rewired) must receive its decrypted credential as an explicit argument
  from a caller that resolved it via `effective_client_id()` (Stage 2) —
  never read a credential via any global/env-var fallback. Grep the
  finished implementation for `Sys.getenv("SAP_` before calling this stage
  done — that pattern should have zero matches left anywhere in the repo.
- The self-serve UI and the staff-jump path use the *exact same* server
  functions with the *exact same* `client_id` resolution
  (`effective_client_id()`) — there is no separate "staff mode" branch that
  takes a different, less-checked path to write a credential. This is the
  same principle Stage 1 established for tiers: one mechanism, not two
  that are supposed to agree.

### 5. UI placement

Recommend extending the existing **Empresas** module (`R/empresas_module.R`,
the `Grupo > Empresas` nav tab) rather than adding a new top-level tab —
companies and their ERP connections are conceptually one screen, and this
reuses UI real estate rather than adding more nav clutter. For each company
row, add an "ERP" action that opens a form (built from the connector
registry, §2) to add/edit/test/delete that company's connection(s).

Before starting: verify whether `R/settings/settings_companies.R` (reached
via the gear-icon Settings modal, `settings_hub.R`) is still a live,
separately-reachable company-management surface, or dead/redirected code —
`settings_hub.R` has one comment suggesting it now redirects to a merged
"Socios" tab, which reads like it's no longer the authoritative surface,
but this wasn't fully confirmed during this audit. If it *is* still live,
flag it back rather than silently duplicating the ERP UI in two places or
guessing which one to extend.

Visibility: this is ordinary client-facing functionality, not a
Hopdesk-internal tool — it does not belong in Stage 2's
`TIERS_STAFF_ONLY_TABS` hiding mechanism at all. It should follow the same
pattern as the existing financial tabs (`FINANCIAL_TABS` in `app.R`):
invisible for a staff-at-home session (nothing to configure — `hd-admin`
has no companies), visible for a native client session and for staff mid-jump,
exactly like Calendario/Vencidos/etc. already behave.

Access: this stage's "add/edit/test/delete connection" actions should be
gated the same way Stage 1 already gates `dev`-tier-and-above client
actions (reuse `tier_rank()`/`is_staff_tier()` from the registry — a
`finance`/`analysis`/`admin` user should not be able to touch ERP
credentials, only `dev` on the client side, or staff mid-jump).

### 6. Migrate Networks' real credentials in, pre-filled

Read the 5 SAP company blocks currently in `.Renviron`
(`SAP_NG_*`, `SAP_NTS_*`, `SAP_NCS_*`, `SAP_NL_*`, `SAP_NRS_*` — url,
company, user, password, ssl_verify each) and write them into
`networks/erp_connections.rds` as 5 `sap_b1_service_layer` rows (encrypted,
per §3), one per company, as part of this stage's own migration step — not
left for Mouse to retype. After migration, `sap_api.R` must read from this
new store exclusively; remove the `SAP_{INITIALS}_*` env var reads from
`.Renviron` and from code once the new path is verified working end to end
against live SAP (do not remove the old path until the new one has been
proven against the real SAP server — this is production financial data).

Also create an (empty) `hopdesk/erp_connections.rds` for the `hopdesk` test
client, consistent with every other client folder — even though it'll
likely stay empty for a while.

## Explicitly out of scope for this stage

- Any second `erp_type` beyond `sap_b1_service_layer` — the registry
  pattern is built to support one later, but do not invent Oracle-specific
  fields now.
- Moving from one shared `HOPDESK_SECRETS_KEY` to per-client keys or KMS —
  noted as a comment, not built.
- Resolving whether `settings_companies.R` is dead code — flag it, don't
  fix it here.

## Test plan

**Automated:**
- Round-trip: `encrypt_secret(list(user="x", password="y"))` then
  `decrypt_secret(...)` returns the identical list. Tampering with even one
  byte of the ciphertext causes `decrypt_secret` to fail loudly (proves
  GCM's tamper detection is actually wired in), not silently return garbage.
- A connection saved under `networks/erp_connections.rds` with
  `client_id = "networks"` is rejected (loud error, not silent drop) if
  code attempts to write it while `effective_client_id()` resolves to
  anything else — simulate a session at a different client attempting the
  write.
- Loading `hopdesk/erp_connections.rds` never returns any row whose
  `client_id` isn't `"hopdesk"` — simulate a corrupted file with a stray
  foreign row and confirm the defense-in-depth assertion in §4 fires.
- A `finance`-tier or `analysis`-tier session cannot call the
  add/edit/test/delete handlers (server-side reject, not just UI hiding).
- After migration, `grep -rn "Sys.getenv(paste0(\"SAP_\"" R/` returns zero
  matches in the final diff.

**Manual** (coordinate timing with Mouse — this touches production SAP
credentials):
1. Before touching anything live, confirm with Mouse when it's safe to
   test against the real SAP server (off-hours, or confirm it's read-only
   and safe to hit anytime).
2. As a `dev`-tier Networks user: open the new ERP UI on the `NG` company,
   confirm the 5 migrated companies' connections show up correctly
   labeled, edit one field, save, use "Test connection," confirm it
   reports success against the real SAP server.
3. As Bunny (hopdesk), jump into `networks`, confirm the exact same UI and
   actions work identically to the `dev` session's experience.
4. Confirm a `finance`-tier session (tesoreria) does not see the ERP
   actions on the Empresas screen at all.
5. Confirm Calendario/Vencidos/etc. still pull real SAP data correctly
   after the credential source has been switched from `.Renviron` to the
   new encrypted S3 store — this is the real end-to-end proof the migration
   worked, not just that the new UI saves rows.

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\saas_rebuild\ARCHITECTURE.md and
docs\saas_rebuild\STAGE_3_ERP_CREDENTIALS.md in full before writing any
code. Confirm you're branching off saas-checkpoint-2026-07-15 (Stages 1+2
are already merged into it) — if stage-3-erp-credentials doesn't already
exist as a branch off that base, create it; if it does, continue on it.

Implement Stage 3 exactly as specified: the erp_connections.rds schema,
the R/erp_connector_registry.R extension point (sap_b1_service_layer only —
do not invent a second ERP type), R/secrets_encryption.R (AES-256-GCM via
the openssl package, master key from a new HOPDESK_SECRETS_KEY env var),
the isolation guarantees in §4, the UI extension to R/empresas_module.R
(after first checking whether R/settings/settings_companies.R is still a
live, separate surface — report back if it is, don't guess), the
FINANCIAL_TABS-style visibility (not the Stage 2 staff-only-hiding
mechanism — this feature is client-facing), and the Networks credential
migration in §6.

This stage touches real production SAP credentials. Before running any
live "test connection" call against the real SAP server, stop and confirm
timing with Mouse — do not assume it's safe to hit anytime. Do not remove
the old SAP_{INITIALS}_* env var reads from .Renviron or code until the new
path has been proven working end to end against live SAP data, per the
Manual test plan's step 5.

Work in small, reviewable increments, starting with the encryption
round-trip and the isolation-guarantee tests before building the UI on top
of them. Run the full existing test suite after each logical change.

When done, stop and report back what changed file by file, whether
settings_companies.R turned out to be live or dead, and the manual test
plan from this document (including the SAP-timing question) for Mouse to
run before merging. Do not merge to master or the checkpoint branch
yourself, and do not touch .Renviron's real SAP values beyond reading them
once for the migration step.
```
