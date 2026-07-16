# Stage 3b — ERP Live Cutover (small, focused follow-up to Stage 3)

Read `STAGE_3_ERP_CREDENTIALS.md` first. Stages 1–3 are merged into
`saas-checkpoint-2026-07-15` and confirmed passing (254/254). Branch this
stage off that branch.

## Why this exists, and why it's deliberately small

Stage 3 built the self-serve encrypted credential store and proved it works
(Mouse tested live: 5 Networks connections correct, "Test connection"
succeeded against real SAP). But the actual Calendario/Vencidos/etc. data
fetch still reads credentials from the old hardcoded `.Renviron`
`SAP_{INITIALS}_*` vars — the new store isn't in the live path yet. This
stage closes that one gap, and only that gap.

**Mouse's explicit priority for this stage: do not lose an ERP connection,
under any circumstance, and don't make anything permanently unrecoverable.**
Two safety nets already exist and were verified live before writing this
doc — don't rebuild them, just rely on them and say so in code comments:
- Soft-delete only, confirmed in `R/empresas_module.R`'s delete handler —
  `deleted`/`deleted_at` are set, no row is ever removed from the data frame.
- S3 bucket versioning is confirmed **Enabled** on `antiguedad-rds-prod` (run
  `scripts/check_versioning.R` yourself to re-confirm before starting) —
  every write is recoverable via `scripts/emergency_rewind.R` even in a
  worst case.

This stage adds a third layer on top of those two: the cutover itself must
be a graceful, auto-falling-back change, not a hard replace.

## Design

### 1. `.sap_creds(initials)` becomes fallback-aware, not replaced

Today `.sap_creds(initials)` resolves credentials purely from
`Sys.getenv(paste0("SAP_", initials, "_..."))`. Change it to:

1. Resolve `effective_client_id()` for the current session and look up an
   active, non-deleted `sap_b1_service_layer` connection for this company
   in `load_erp_connections(client_id)` (Stage 3).
2. If found: decrypt via `decrypt_secret()`. If decryption succeeds, use it
   — and log (at message level, not silently) `"[SAP] credentials for {X}
   resolved from erp_connections store"`.
3. If decryption **fails** (wrong key, corrupted blob, anything): do not
   crash the fetch. Log a **loud warning**
   (`"[SAP] WARNING: erp_connections credential for {X} failed to decrypt
   (%s) — falling back to legacy .Renviron credentials. Investigate."`)
   and fall through to step 4 exactly as if no row had been found. A
   decrypt failure must never silently either break the calendar or pass
   unnoticed — it should keep working via fallback AND be visible in logs.
4. If no active connection was found for this company (or step 3 fell
   through): fall back to the existing `SAP_{INITIALS}_*` env var lookup,
   unchanged — log `"[SAP] credentials for {X} resolved from legacy
   .Renviron (no erp_connections row / decrypt failed)"` so it's always
   clear which path served any given fetch.
5. If *neither* path has anything for this company: same error behavior as
   today (missing credentials) — no change there.

This means: for Networks' 5 already-migrated companies, the new store will
serve every real fetch starting the moment this ships, in production, with
real data — but if anything is wrong with the new store for a given
company, the old path picks up instantly and nobody's calendar goes empty.

### 2. Add a "Restore" action for soft-deleted connections

Given how much this matters to Mouse, add a small UI affordance in the ERP
section of `R/empresas_module.R`: a "Ver eliminadas" (or similar) toggle
that lists soft-deleted connections for that company and a "Restaurar"
button that sets `deleted = FALSE`, `deleted_at = NA`, `active = TRUE` (let
the user re-confirm the fields still look right afterward, since it may
have been deleted for a reason). Small addition, but directly answers "we
really do not want any connection to get lost."

### 3. Visibility during the transition

Add a small, unobtrusive indicator (not a big new UI — keep it minimal per
Mouse's screen-real-estate preference) somewhere near the ERP section
showing, per company, which path last served a fetch ("Fuente: conexión
guardada" vs "Fuente: variable de entorno (respaldo)") — pulled from
whatever the `.sap_creds()` resolution logged, not a new tracking
mechanism. This lets Mouse (or Hopdesk support) glance at it during the
first few real trading days and build confidence before ever considering
removing the fallback.

## Explicitly out of scope for this stage

- **Do not remove `SAP_{INITIALS}_*` from `.Renviron` or from
  `.sap_creds()`.** That fallback is the point of this stage's design —
  removing it is a distinct future decision Mouse makes explicitly, once
  he's watched the new path serve real fetches successfully for a while.
  Do not suggest doing it "since it's proven now" — that's not your call
  to make in this stage.
- No second ERP type, no schema changes beyond what Stage 3 already built.
- No change to the isolation guarantees from Stage 3 — reuse
  `load_erp_connections()`/`decrypt_secret()` exactly as they exist.

## Test plan

**Automated:**
- Simulate a company with an active, correctly-decryptable connection in
  the store: `.sap_creds()` returns the store's values, not the env var's
  (use two deliberately different fake values to prove which one won).
- Simulate a company with a connection row that fails to decrypt (corrupt
  the ciphertext in a test fixture): `.sap_creds()` falls back to the env
  var value and does not throw.
- Simulate a company with no row in the store at all: falls back to the
  env var path exactly as today, unchanged behavior.
- Simulate a company with neither: same error as today's baseline
  (regression check — don't change this failure mode).

**Manual** (production data, coordinate timing with Mouse exactly like
Stage 3 — off-hours, confirm before running):
1. Before anything: run `scripts/check_versioning.R` yourself and confirm
   `Enabled` again right before starting, don't rely on this document's
   record of it.
2. Deploy the change, then open the Calendario tab as a Networks user and
   confirm real invoice data still appears exactly as before — this is the
   actual proof, not just that a test passed.
3. Check the logs for each of the 5 companies and confirm they show
   "resolved from erp_connections store," not the legacy fallback message —
   if any company shows the fallback message, stop and investigate why
   before considering this done (it likely means that company's migrated
   row has an issue worth understanding, even though the fallback kept the
   calendar working).
4. Test the new "Restore" button: soft-delete a throwaway test connection
   (not a real Networks one), confirm it disappears from the active list,
   restore it, confirm it's back and usable.
5. Leave the fallback in place and ask Mouse to keep an eye on the
   source-indicator (§3) over the next few real business days before any
   future conversation about removing `.Renviron`'s SAP values for good.

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\saas_rebuild\STAGE_3_ERP_CREDENTIALS.md
and docs\saas_rebuild\STAGE_3B_ERP_LIVE_CUTOVER.md in full before writing
any code. Branch off saas-checkpoint-2026-07-15 (Stages 1-3 already
merged), name the branch stage-3b-erp-live-cutover.

Before writing any code, run scripts/check_versioning.R yourself and
confirm S3 versioning is still Enabled on the production bucket. Also
re-read R/empresas_module.R's delete handler and confirm it is still
soft-delete only. Report both back before proceeding — these are the
safety nets this whole stage's design leans on, so don't assume they're
still true, verify them fresh.

Implement exactly the three items in this document's Design section:
the fallback-aware .sap_creds() rewrite (with the loud-warning-on-decrypt-
failure behavior), the Restore button for soft-deleted connections, and
the small source-indicator UI. Do not remove the legacy SAP_{INITIALS}_*
env var path from .Renviron or from the code — it is the fallback safety
net this stage exists to build on top of, not something to clean up.

This stage touches the real, live Calendario data path for a production
client. Stop and confirm timing with Mouse before testing against real
SAP, exactly as in Stage 3. After deploying, check the logs for all 5
Networks companies and confirm which credential source served each one —
report this back explicitly, don't just say "it works."

Run the full existing test suite after each logical change. When done,
stop and report back file by file, plus the manual test plan from this
document (including the per-company source-check) for Mouse to run before
merging. Do not merge to master or the checkpoint branch yourself, and do
not remove or modify any real credential in .Renviron.
```
