# Stage 8 — Comprehensive Cross-Tenant Isolation Sweep

Branch off `master`. This is the largest stage since the original rebuild —
take it in clearly reviewable increments, not one giant commit.

## The bug family this stage exists to hunt down, generalized

Across Stages 2 and 4 we found and fixed four distinct instances of the
same underlying mistake: **code that infers "who is this session, and
whose data should it touch" from something other than the session's own
resolved identity** (`current_user_info()`, `effective_client_id()`,
`home_client_id()`, `is_staff`) — most often by reading the `CLIENT_ID`
environment variable directly (which only ever answers "which deployment
is this," never "who's logged in right now, in this one shared
deployment"), or by using a shared, process-wide cache/variable that isn't
keyed per session/client at all.

Mouse's own words on why this matters enough to sweep comprehensively,
rather than wait for the next one to surface during testing: *"I'm seeing
the data correctly but I'm not seeing what's truly behind, and that's
where the real errors are generated."* **Manual UI testing cannot catch
this bug family** — a single person clicking through the app as one user
has no way to observe whether a *different, simultaneous* session's data
leaked somewhere. This is why Part B below (an automated concurrent-session
test) is not optional polish — it is the actual verification mechanism for
this entire stage. Do not substitute manual click-through testing for it.

## Part A — Pattern-based audit (efficient: grep and judge, don't read everything)

Search the **entire** codebase (not just the files touched by Stages 1-5)
for each pattern below. For every hit, classify it as either **safe**
(explain why in one line) or **suspicious/confirmed bug** (fix it,
following the same approach already used in Stages 2 and 4 — thread the
actor's real identity through explicitly, never rely on an env-var or
shared-state fallback for a live routing/data decision).

1. **`Sys.getenv("CLIENT_ID")` / `Sys.getenv('CLIENT_ID')`** — every
   occurrence, project-wide (the earlier grep during this project only
   covered SaaS-relevant files; re-run it against everything, including
   `bancos_module.R`, the `pasivos_*` family, `forecasting_*`,
   `ledger_module.R`, `search_module.R`, `proveedores_module.R`,
   `treasury_map_module.R`, and the `R/settings/*` files).
   - **Safe pattern**: process-level bootstrap constants (`IS_ADMIN_DEPLOYMENT`
     in `global.R`), a function's default *parameter value* where every
     actual call site already passes the real identity explicitly (verify
     this by checking every call site, not by assuming — this was the
     exact mistake in the pre-Stage-4 `log_action()`).
   - **Suspicious**: any function that makes a data-loading, routing, or
     permission decision and reads this env var as a stand-in for "who is
     the current actor," especially without an explicit `client_id`/
     `viewer_home_client_id`-style parameter a caller could override.
2. **`observeEvent(..., once = TRUE)` combined with `req(...)` inside the
   handler body** — the exact race already found and fixed four times in
   `app.R` (Stage 1's `redirected_to_staff_home`, Stage 4's
   `home_client_id` init / emergency-lock check / force-password-change
   gate / `hop_grants_db` loading). Search every module file, not just
   `app.R` — a module with its own `once=TRUE` + `req()` pattern on a
   module-scoped reactive has the identical failure mode. Fix using the
   same manual-guard-flag pattern already established (see
   `STAGE_1_TIER_PERMISSION_MODEL.md` and the `683bbd8` commit for the
   reference implementation) — do not reintroduce `once = TRUE` anywhere
   this pattern is found.
3. **Process-wide mutable state that isn't keyed per client/session** —
   the SAP-cache shape of bug (Stage 2 Part B). Search for `.GlobalEnv$.`
   assignments and any top-of-file (outside a function) mutable `<-`
   state, then check: does more than one client/session's data ever pass
   through this same variable? If so, is it keyed by client id (like the
   fixed `.sap_global_cache`) or one shared blob (the bug shape)?
4. **Every function with a `client_id = NULL`-style parameter whose
   default falls back to the env var or a shared value** — enumerate all
   of them (`read_audit_log`, `load_erp_connections`, `log_action`,
   `read_contacts`/`write_contacts`, `load_grupos`, and any others found)
   and, for each, verify **every call site** actually passes the real
   actor identity rather than relying on the default. This is exactly how
   the pre-Stage-4 `log_action()` bug happened — 24 call sites, 7 silently
   relying on the fallback. `read_contacts`/`write_contacts` and
   `load_grupos` were spot-checked during this project and found fine
   (callers already pass `client_id` explicitly) — re-verify them anyway
   as part of this comprehensive pass rather than trusting that earlier
   spot-check, and check every other function of this shape that wasn't
   checked before.
5. **Places computing "is this staff / is this a jump" by comparing two
   client-id-shaped strings instead of reading `is_staff`/
   `jump_client_id() != NULL` directly** — even where currently harmless
   (see `R/sync_bus.R`'s `in_jump_context`, traced during this project and
   found benign but mislabeled), flag and clean these up for consistency —
   a "harmless today" comparison like this is exactly the shape that turns
   into a real bug the next time someone edits the surrounding code without
   knowing its true meaning.

## Part B — Concurrent-session behavioral test harness (the real safety net)

Build a **new, reusable test utility** — not a one-off test, a piece of
test infrastructure future stages should keep using — that simulates two
(or more) sessions with different identities
(`current_user_info()`/`home_client_id()`/`jump_client_id()` values for
each) sharing the same R process state, the same way real production
sessions do, and drives them **interleaved** against the app's real
data-loading functions and reactive logic, then asserts:

- Session A's returned data/side effects never contain session B's
  `client_id`, and vice versa.
- Any S3 write attributed to session A is never attributed to session B's
  client folder, and vice versa (reuse the existing isolation-assertion
  style already established in Stage 3's `.assert_erp_connections_isolated()`
  and Stage 4's `read_audit_log_scoped()` — same philosophy, generalized
  into a reusable test helper rather than one-off checks).

**Prove the harness has real teeth before trusting it**: temporarily and
locally (never commit this) revert one of the already-fixed bugs — e.g.
the pre-Stage-2 unkeyed `.sap_global_cache` — and confirm the new
behavioral test fails against the reverted code. Then confirm it passes
again once reverted back. A test harness that would pass either way is
worthless; this step is not optional.

**Scope pragmatically, but build it to grow**: you cannot exhaustively
drive every reactive in a 15k+ line app in one stage. Build the harness as
a generic, reusable framework first, then apply it to the highest-value
targets: SAP/ERP data loading, `bancos_module.R`, the `pasivos_*` family,
`forecasting_*`, and the audit log — the modules that touch real financial
data or were already found fragile once. Structure the harness so more
targets can be added later as a standing regression suite, not a
one-and-done exercise — this is meant to be Mouse's actual ongoing
insurance against this bug family recurring, not a single audit event.

## Explicitly out of scope

- Fixing every *other* kind of bug you happen to notice while sweeping —
  stay focused on this one bug family. Note anything else found, but
  don't fix it here (flag it back, the same way Stage 2's `sync_bus.R`
  finding was flagged rather than fixed on the spot).
- Rewriting `sync_bus.R`'s `in_jump_context` logic to something entirely
  different — just make it consistent with the established `is_staff`/
  `jump_client_id()` pattern, don't redesign the sync mechanism itself.

## Test plan

This stage's own deliverable (Part B) largely *is* its test plan. In
addition:
- Every fix from Part A gets its own targeted regression test (mirror the
  existing style — e.g. `test_saas_log_action_scoping.R`'s static-analysis
  approach for "every call site passes the required parameter").
- Full existing suite still green after every change.
- The Part B harness's "proven against a reverted known bug" check (above)
  stands as this stage's core acceptance criterion — report the before/after
  result explicitly, don't just say "tests pass."

**Manual**: light-touch only, per Mouse's own instruction not to rely on
manual UI checking for this bug family — a basic sanity pass (log in as a
couple of different accounts, confirm normal usage still works) is enough;
do not ask Mouse to hunt for isolation bugs by eye.

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\saas_rebuild\ARCHITECTURE.md and
docs\saas_rebuild\STAGE_8_ISOLATION_SWEEP.md in full before starting. This
is the largest stage so far - work in small, clearly separable commits,
not one giant one.

Do Part A (the pattern-based sweep across the WHOLE codebase, not just
previously-touched files) first, fixing each confirmed bug using the same
"thread the real identity through explicitly" approach already established
in Stages 2 and 4. Report every hit found, whether classified safe or
fixed, even ones you conclude are safe - Mouse wants the full picture, not
just the fixes.

Then build Part B, the reusable concurrent-session test harness. Prove it
actually catches a real bug before trusting it, by temporarily (locally,
never committed) reverting one already-fixed bug and confirming the
harness fails against the reverted code, then passes again once restored -
report this before/after result explicitly.

Apply the harness to the highest-value targets listed in Part B. Build it
to be extended later, not as a one-time exercise.

Do not fix unrelated bugs you notice along the way - flag them back
instead. Run the full existing test suite after each logical change.

When done, report back: every pattern-A hit found and its classification,
every fix made, the harness's before/after proof, and a manual sanity-test
list for Mouse (not an isolation-bug hunt - he's already told you not to
rely on manual checking for that). Do not merge to master yourself.
```
