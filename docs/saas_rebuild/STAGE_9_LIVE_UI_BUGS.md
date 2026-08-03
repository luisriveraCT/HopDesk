# Stage 9 — Live UI Bugs Found During Hands-On Testing (2026-08-03)

**Status:** Ready to implement. Written by a Claude Code session that just
finished Stage 6 (the log_action completeness sweep) and does NOT implement
any of the fixes below — that's your job. This document exists so you don't
have to re-derive the diagnosis from scratch; treat every "Finding" below as
verified against the actual code (file:line given), and every "Hypothesis" as
a strong lead that still needs your own confirmation before you build a fix
on top of it.

**Read first, in this order:**
1. `docs/saas_rebuild/ARCHITECTURE.md` in full — the multi-tenant model, the
   "vacuum" isolation guarantee, and §3's explicit decision: **one shared
   deployment, not one per client**. This last point is load-bearing for
   Issue 4 below.
2. `docs/saas_rebuild/STAGE_8_ISOLATION_SWEEP.md` — written, **never
   executed** (confirmed via `git log --oneline -- docs/saas_rebuild/STAGE_8_ISOLATION_SWEEP.md`
   and cross-checked against full repo history — no commit does Part A or
   Part B of that stage). It independently predicted the bug family Issue 4
   below is hands-on evidence of. You may end up doing a scoped slice of
   Stage 8 as part of fixing Issue 4 — that's expected, not scope creep.
3. `docs/LEDGER_INTEGRITY_MASTER_PLAN.md` — for the established conventions
   (test style, comment style, `if (FALSE) {...}` retirement pattern,
   `.normalize()`/schema-migration approach). Skim, don't read cover to
   cover.
4. `git log --oneline -20` before you start, and re-run it again before each
   stage below. Other Claude Code sessions and Mouse himself are actively
   working on this same repo in parallel — do not assume the state this
   document describes is still exactly current. Where this document gives a
   file:line reference, treat it as "this is where it was when I checked,
   re-verify" not "this is guaranteed still true."

## Context you need that isn't in the architecture doc

- **Testing convention established this project:** every stage gets its own
  batch of tests (the project's own bar, stated explicitly by Mouse, is "at
  least 50 tests per stage, more if warranted" — static-analysis/regex-scan
  tests in the style of `tests/test_saas_log_action_scoping.R` and
  `tests/test_log_action_completeness_scan.R` count, but for anything
  claiming to fix a *live, interactive* bug, back it with a **chromote**
  (headless-Chrome, R package) harness that actually drives the browser —
  static analysis alone has repeatedly missed real bugs in this app's
  history (see the `ab_rows` shape bugs in the master plan, Stages 20-21).
  For cross-session bugs (Issues 2 and 4 below), the harness needs **two
  concurrent sessions against the same running app**, not one — a single
  browser tab cannot observe what a second session sees.
- **Full regression suites that must stay green after every change:**
  `Rscript tests/_run_confirmed_logic.R` (862 checks as of this writing) and
  `Rscript tests/_run_saas.R` (488 checks). Run both after every stage, not
  just at the end.
- **Audit log ("Actividad"/"Bitácora"):** `log_action()` in `R/app_audit.R`
  is the ONE real audit trail the app has (Stage 5 of the master plan fixed
  a second, invisible one in Pasivos — `pasivos_log_audit()` now dual-writes
  into this same store). Stage 6 (just finished, see `fe8f6f5`..`f1721e1`)
  swept the whole app for mutating handlers missing a `log_action()` call
  and closed the last one — `tests/test_log_action_completeness_scan.R` now
  fails the suite if a new one ships unlogged. **A commit landed after that
  work** (`ef714a1`) that changed `log_action()` from a synchronous S3
  write to a deferred, batched one (`later::later()`, 2s delay) because the
  synchronous version was adding multi-second latency to every single
  action app-wide. Know this exists before you go looking for latency
  causes — it already fixed one real source of "slow af," but (see Issue 4
  below) it is **not** what's causing the cross-session propagation delay
  Mouse is reporting; that's a different mechanism (`sync_bus.R`'s poll +
  modal re-render), not `log_action()`.
- **Client-scoping convention:** every loader/`save_*()` takes
  `client_id = NULL` and falls back to `Sys.getenv("CLIENT_ID")`, and every
  `log_action()`/`register_synced()` call threads `client_id`/
  `viewer_home_client_id` explicitly. This convention exists specifically
  because of the isolation requirement in ARCHITECTURE.md §1-2. Any fix you
  write must preserve it, and Issue 4 below is specifically about verifying
  it's actually being *respected*, not just present as a parameter.
- **Git workflow for this stage:** work on a dedicated branch, e.g.
  `fix/stage9-live-ui-bugs`, not directly on `master`. Commit after each
  sub-stage below (they're independent enough to land separately), push the
  branch to GitHub regularly so Mouse can watch progress. **Do not merge to
  master yourself** — when everything below is done and verified, tell
  Mouse it's ready and let him review + merge. This mirrors
  `STAGE_8_ISOLATION_SWEEP.md`'s own instruction, already an established
  convention in this repo.
- **Documentation-of-failure convention (new requirement for this stage,
  please start it now and keep it going):** append to
  `docs/saas_rebuild/STAGE_9_ATTEMPT_LOG.md` (create it) every time an
  approach you try turns out wrong or has to be reverted/reworked — what you
  tried, why it seemed reasonable, and specifically why it didn't work.
  This mirrors the master plan's own "Stage N correction" sections (see
  "Stage 21 correction" in `LEDGER_INTEGRITY_MASTER_PLAN.md` for the tone/
  format) but as its own file so it doesn't get lost in a huge master doc.
  The point: if you (or a future session) is ever tempted to retry something,
  this file should already have the answer. Also comment every non-obvious
  fix in the code itself, in this project's existing style — grep any file
  for `found 2026-` to see dozens of examples of the expected tone (state the
  bug, the date/context it was found, and the specific reasoning for the
  fix, not just what the fix does).

---

## Issue A (Mouse's #1 and #5 combined) — "Salir" doesn't log out; a copied URL logs you in forever

**Mouse's words:** *"Salir doesn't actually log me out, it only refreshes the
page."* / *"Being able to copy my url while logged in and then pasting
elsewhere and having it auto-login feels like a massive vulnerability."*

**Finding — confirmed root cause, both symptoms are the same bug:**
`app.R:676-679` sets `keep_token = TRUE` on `shinymanager::secure_server()`.
This was Stage 0 of the real-time-sync work (see the comment right above it,
and commit `4d9fbe2`) — before that fix, shinymanager stripped its own auth
token from the URL right after login, so *any* browser refresh looked like a
forced logout. The trade-off was written down explicitly at the time: *"the
token now sits visibly in the browser URL, so a copied/shared/bookmarked link
would let someone else assume that session until the inactivity timeout."*
That trade-off was accepted for the "refresh doesn't kick you out" win — but
nobody at the time checked whether **explicit logout still worked**, and it
doesn't:

`app.R:655-662`:
```r
observeEvent(input$btn_logout, {
  shinyjs::runjs("
    document.cookie.split(';').forEach(function(c) {
      document.cookie = c.replace(/^ +/, '').replace(/=.*/, '=;expires=' + new Date().toUTCString() + ';path=/');
    });
    location.reload();
  ")
}, ignoreInit = TRUE)
```
This clears cookies and reloads the page. It does **nothing** to the
token — which lives in the URL query string, not (necessarily) a cookie —
so `location.reload()` reloads the exact same URL, shinymanager sees the
still-valid, still-unrevoked token, and logs the user right back in. This is
one bug, not two: **"Salir doesn't log out"** and **"a copied URL never
expires until 15 min of inactivity"** are the same root cause (no server-side
token revocation exists anywhere in this app, and the client-side logout
doesn't strip the token from the URL either).

**What to investigate before writing a fix:**
1. Read `shinymanager`'s actual installed source
   (`system.file(package = "shinymanager")`, specifically the `secure-server.R`/
   token-store internals) to find: is there an exported or internal function
   to invalidate one specific token from its in-memory store? Does
   `secure_server()`'s returned reactive expose anything logout-shaped
   already that just isn't being called? A previous session (per the
   comment at `app.R:664` — "confirmed by reading the installed shinymanager
   source") already did some of this reading for the *login* side; you need
   the *logout/revocation* side specifically.
2. Confirm exactly what shinymanager validates against on each request: the
   URL token param, a cookie, or both. This determines whether "clear
   cookies" was ever doing anything useful at all, or was dead code from the
   start.
3. Design intent to preserve: refresh must still NOT force a re-login (don't
   regress Stage 0). Explicit "Salir" must actually end the session — both
   killing whatever server-side validity the token has, and making sure the
   URL the browser is left on no longer contains a live token (so if the tab
   stays open or the URL gets copied afterward, it's already dead).
4. Mouse's own words define "good enough": a small trusted internal team,
   15-minute inactivity timeout already exists and is *not* being
   complained about — the complaint is specifically that clicking Salir (or
   letting the session go idle) should reliably and immediately kill that
   specific token, not that URL-embedded tokens are unacceptable in
   principle. Don't over-engineer a bigger auth rearchitecture than this
   calls for; if you find yourself wanting to rip out `keep_token` entirely,
   stop and flag that back rather than doing it — that would reintroduce the
   Stage 0 bug and needs an explicit decision, not a unilateral one.
5. Also worth confirming while you're in this code: Mouse noted that
   spam-clicking the logout button "eventually" logs out. Check whether this
   is simply "the 15-minute timeout happened to elapse during the clicking"
   (coincidence, nothing to fix) or something more specific to repeated
   `location.reload()` calls racing each other. Low priority relative to the
   main fix; a paragraph in your final report is enough, don't rabbit-hole
   on it. Note it may be entangled with Issue B below (see the last
   paragraph of Issue B's write-up).

**Suggested test plan:** a chromote-driven test that (a) logs in, (b) copies
the current URL, (c) clicks Salir, (d) asserts the ORIGINAL tab is back at
the login screen, and (e) asserts navigating to the COPIED url in a fresh
session/context no longer auto-authenticates. Plus a normal refresh test
(already exists somewhere from Stage 0 — check `tests/test_auth_keep_token.R`
before writing a new one; extend it rather than duplicating).

---

## Issue B (Mouse's #2) — A stale, closed modal reopens itself from a completely unrelated action

**Mouse's words:** *"I was testing the live update feature and as soon as I
added a tag to an item in 'Vencidos' I got this window [Calendario's Aug 7
day-view modal] ... this bug happens across the entire app ... it also
happens on logout spam-clicking (shows Aug 7 again first)."* Mouse also
stated an explicit design preference to keep in mind while fixing this:
modals should **not** auto-close or reset scroll position just because a
single action happened inside them (users often make several changes in a
row) — the bug isn't "the modal ever updates live," it's "a modal the user
already left reopens itself for no reason, anywhere in the app."

**Finding — a very likely root cause, found by reading the exact
open/close-tracking code, not by guessing:**

`R/ledger_module.R:654` declares `modal_open <- reactiveVal(FALSE)` — this
is per-module-instance state; `ledgerModuleServer` (the module this lives
in) is instantiated **twice**, once for AR and once for AP, so there are
actually two independent `modal_open`/`modal_ctx()` pairs, each namespaced
by Shiny's module system.

The intended close-detection mechanism, `R/ui_components.R:1123-1128`:
```js
document.addEventListener('hidden.bs.modal', function() {
  if (window.Shiny) Shiny.setInputValue('cal_day_modal_closed', Math.random(), {priority: 'event'});
});
```
This is a **document-level, non-namespaced** JS listener — it fires
`Shiny.setInputValue('cal_day_modal_closed', ...)` using the literal string
`'cal_day_modal_closed'`, not anything run through `ns()`.

The consumer, `R/ledger_module.R:719-721`:
```r
observeEvent(input$cal_day_modal_closed, {
  modal_open(FALSE)
}, ignoreInit = TRUE, ignoreNULL = TRUE)
```
This lives inside `moduleServer(id, function(input, output, session) {...})`
(`R/ledger_module.R:36`). **Inside a Shiny module, `input$x` is automatically
scoped to that module's namespace** — this `input$cal_day_modal_closed` is
actually listening for `Shiny.setInputValue('<namespace>-cal_day_modal_closed', ...)`,
which nothing ever sends. The JS sends the bare root-level name; the R side
listens for the namespaced one. **These two names almost certainly never
match**, meaning this `observeEvent` may never fire at all, for either the
AR or AP module instance.

`grep -rn "rootScope" R/*.R` returns zero hits anywhere in this codebase —
the well-known Shiny technique for reading a root/document-level input from
inside a module (`session$rootScope()$input$x`) is not used anywhere, which
is consistent with this being a genuine oversight rather than an
intentionally-different-but-working approach you're misreading.

**If this diagnosis is right, the consequence is:** `modal_open()` starts
`FALSE`, flips to `TRUE` the first time a user opens *any* day in Calendario
(`R/ledger_module.R:700`, inside the "single render gate" `observeEvent(modal_ctx(), ...)`),
and — because the close signal never arrives — **never flips back**, for the
rest of that browser tab's session. The live-refresh observer added in
Stage 2 (`R/ledger_module.R:735-753`, watching
`moves_db/manual_inv/tags_db/papelera_rv/bancos_confirmados/abonos_db/pasivos_provisions_db`)
checks `if (!isolate(modal_open())) return()` before doing anything — with
`modal_open()` stuck `TRUE`, *any* future change to *any* of those seven
datasets, from *anywhere in the app*, re-triggers `.refresh_ctx_detail(ctx)`
using the **stale `ctx`** (still pointing at whatever day was last opened —
Aug 7, in Mouse's case) and calls `modal_ctx(new_ctx)`, which re-fires the
"single render gate" and calls `showModal()` again — visibly reopening a
modal the user closed however long ago, for a day that has nothing to do
with what they're currently doing.

This exactly matches Mouse's reproduction: tag something in Vencidos →
`bump_sync_version("tags_db")` fires (tags are correctly meant to sync
live — that part is not the bug) → `shared$tags_db()` changes → Ledger's
stuck-open live-refresh observer fires → Aug 7 reopens.

**What to verify before fixing (don't take this diagnosis on faith):**
1. Add temporary logging/breakpoints (or a chromote test that opens a day
   modal, closes it via clicking outside, and inspects
   `session$input$cal_day_modal_closed` / the module's internal `modal_open()`
   value via a debug output) to confirm the signal truly never arrives.
   Confirm this **before** writing the fix — if you're wrong about the
   namespace mismatch, the real cause is something else and you need to find
   it, not patch around a misdiagnosis.
2. Check whether this exact pattern (a root-emitted JS signal consumed by a
   namespaced module's `input$`) was copied anywhere else in the app. Search
   for other `_modal_closed`-shaped inputs or other `hidden.bs.modal`
   listeners.
3. Check whether Pasivos' own Stage 2 live-refresh (`R/pasivos_module.R`,
   `output$pcm_conv_body <- renderUI({...})` reading `shared$pasivos_provisions_db()`/
   `shared$manual_inv()` directly) has the same failure mode. On a first
   read it looks architecturally different and safer — it's a `renderUI`
   that updates the *content* of an already-open modal reactively, rather
   than an observer that calls `showModal()` again from scratch — so it may
   not be able to "reopen" a closed modal the same way. Confirm this
   yourself rather than trusting this note; if Pasivos modals turn out to
   have their own version of this bug, fix it there too using whatever
   approach you land on for Ledger.
4. Whatever the fix is, it needs to satisfy Mouse's explicit constraint: a
   modal that's genuinely still open and receives a live update from
   elsewhere should still update in place (don't lose that — it's a
   deliberate feature), and should NOT reset scroll position or otherwise
   feel like it "closed and reopened" during that live update. The bug is
   specifically about a *closed* modal reappearing, not about live updates
   to an open one being disruptive. Re-read Mouse's own reasoning in his
   report before designing the fix: *"IF the users actually wanted to close
   the modal it's way too easy they can just click outside... 90% of the
   time users make multiple changes."* A fix that makes modals stop
   updating live to solve this would be solving the wrong problem.

**Suggested test plan:** a chromote test that (a) opens a Calendario day
modal, (b) closes it (via a backdrop click or ESC, not just a "Cerrar"
button, since button-driven closes might go through a different path that
already works), (c) performs an unrelated action elsewhere that bumps one of
the seven watched datasets (e.g. tagging an item in Vencidos), and (d)
asserts no modal is visible afterward. Also test the "still open, gets a
live update" path still works (don't regress Stage 2).

---

## Issue C (Mouse's #3) — Bancos' delete button needs its own papelera-equivalent store

**Mouse's words:** *"Bancos worked perfectly except for the delete button.
replicate a delete button from elsewhere in mechanics and logging function
to keep a log, also it needs to add the movement to 'papelera', displaying
it on the same list in that UI, but in the backend it does need to be in a
separate list because it's the bank movement bin, so it's gotta be different
data and different list from the one used by items. ... you already have the
perfect example of one that works, right in this app."*

**Finding — check this first, it may already be partially/fully fixed:**
As of the current `HEAD`, `R/bancos_module.R`'s bulk-delete handler
(`observeEvent(input$do_eliminar_confirm, ...)`, currently around line 1584)
**does** call `log_action()` already (added in this project's own Stage 6
sweep, commit range `fe8f6f5`..`f1721e1` — specifically the Batch 8 entry for
"Bancos: do_reasignar_confirm, do_import" — though `do_eliminar_confirm`
itself was logged even earlier, per the master plan). And
`papelera_tbl`'s `rows_mov` section (currently around line 3746) **does**
already include every deleted bank movement regardless of source, with a
"Deshacer" (undo) button shown **only** for `fuente == "manual"` rows (by
deliberate design — see the comment right above it: restoring a
TXT-imported row would fight a future re-import of the same statement).

**So there are two possibilities, and you need to figure out which one you're
in before doing anything else:**
1. Mouse tested against a *stale* running R process/deployment that
   predates one or both of those fixes (this app persists its reactive
   environment across `runApp()` calls within a session — see the note in
   memory about this — so an R process started before a code change won't
   pick it up until restarted/redeployed). If so, the actual code gap here
   is smaller than his report suggests, and you should say so explicitly in
   your final report rather than silently "fixing" something already fixed.
2. Mouse is describing a **different delete button** than
   `do_eliminar_confirm` — re-check the exact UI path in the screenshot he
   sent (a confirm modal titled "¿Eliminar 1 movimiento?" with a table
   showing Fecha/Parte/Cargo/Abono, Cancelar/Eliminar buttons) against every
   delete-shaped button in `R/bancos_module.R` to be sure you're looking at
   the same one he was.

**The actual design ask, independent of the above (do this regardless of
which case you're in):** Mouse explicitly wants a **dedicated, separate
backend data store for deleted bank movements** — not the existing
`add_to_papelera()`/`.schema_papelera()` machinery (`R/persistence.R:719-801`),
which is scoped to *invoice-shaped* items (Empresa/Documento/Parte/Importe)
and shared across manual invoices, SAP ghosts, and provisions — because bank
movements are a structurally different object (cargo/abono/banco/cuenta_id,
no Documento/Parte in the invoice sense). He explicitly said *"you already
have the perfect example of one that works, right in this app"* — that's
this exact `manual_inv` + `papelera.rds` lifecycle
(`add_to_papelera()`/`restore_from_papelera()` in `R/persistence.R`, wired up
in `R/manual_entry_handlers.R` and `app.R`'s `me_save` observer). **He wants
you to build the equivalent shape as a new, separate table** (its own
`.schema_bancos_papelera()`-style schema, its own S3 key, its own
`save_*()`/`load_*()` pair, its own `add_to_bancos_papelera()`/
`restore_from_bancos_papelera()`-equivalent functions) — while still
surfacing rows from BOTH stores in the same shared `papelera_tbl` UI so the
user has one place to look, exactly like `rows_mov`/`rows_conf`/`rows_vin`/
`rows_ledger` already do for their own respective sources.

**Open design question — do not decide this unilaterally, flag it back or
make a clearly-labeled judgment call and say so in your report:** should the
new store's "Deshacer" be available for every deleted movement (manual AND
TXT-imported), or should it preserve the existing deliberate restriction
(manual-only, because restoring a TXT-imported row fights re-import)? Mouse's
message doesn't say "undo everything" explicitly — he described the
*storage/display* requirement, not the restore-eligibility rule. The
existing TXT-conflict reasoning (comment at `R/bancos_module.R` ~line 3756)
is sound and was itself a deliberate decision from an earlier stage — don't
overturn it without flagging that you're doing so.

**What NOT to do:** don't retrofit `bancos_movimientos`'s existing
`eliminado`/`eliminado_at` flag-flip into something else unless you have a
clear reason to — Mouse's ask is for an *additional*, separate archive
table alongside/instead of that flag, modeled on the item-level papelera's
shape. If keeping the flag AND adding a real archive table both make sense
(e.g. flag for "hide from the live Libro de Banco view," archive table for
"the permanent papelera record with restore"), that's a legitimate design,
but decide and document it explicitly rather than silently picking one.

**Suggested test plan:** unit tests for the new schema/save/load/add/restore
functions (mirror `tests/` coverage style already used for
`add_to_papelera()`/`restore_from_papelera()` — find and read those tests
first), plus a chromote test: delete a movement, confirm it appears in the
shared papelera UI, confirm (per whatever you decide on the open question
above) whether/how it can be restored, confirm a log_action() entry exists
for both the delete and the restore.

---

## Issue D (Mouse's #4, part 1) — Pasivos provision deletion doesn't log; sweep every delete/move path again

**Mouse's words:** *"I deleted a provision from the Pasivos module and it
worked but did not show on activity, ever, which is a problem please correct
it, review again all ways of deleting items and provisions to ensure they
trigger a log, same with the feature that lets me move the dates (mover)."*

**Context: this is NOT the same code path Stage 5 already fixed.** The
master plan's Stage 5 (commit `e6f15aa`) fixed `pasivos_log_audit()` to
dual-write into the real audit log, and specifically enriched three
observers in `R/pasivos_module.R`: `.pasivos_perform_conversion()`
(converting a provision into a real invoice), `pcm_do_undo_conversion`
(reverting a conversion), and `pcm_do_delete_converted` (deleting an
already-converted item). **None of those three is "delete a still-open,
never-converted provision"** — that's a different action Mouse is
describing, and it may go through a code path Stage 5 never touched (a
different observer in `pasivos_module.R`, something in
`pasivos_wizard_module.R`, or a direct `save_pasivos_provisions()`/
`save_pasivos_liabilities()` call that never routes through
`pasivos_log_audit()` at all).

**What to do:**
1. Find every place in the codebase that deletes a provision or a liability
   (not just converted ones — the still-provisional/open state too), and
   every place that moves a provision/liability's date ("mover"). Don't
   assume it's confined to `pasivos_module.R` — check
   `pasivos_engine.R`, `pasivos_wizard_module.R`, and `pasivos_audit.R`
   too.
2. For each one found, confirm it calls `pasivos_log_audit()` (which now
   correctly dual-writes per Stage 5) or `log_action()` directly, with
   real `before`/`after` detail where the action changes data (matching the
   enrichment pattern Stage 5 established for conversions — see
   `.pasivos_perform_conversion()`'s `content_diff` computation as the
   reference shape, not something to copy verbatim).
3. Re-run `tests/test_log_action_completeness_scan.R` after your fixes and
   confirm it still reports 0 actionable gaps — but don't treat a clean
   scanner run alone as proof; the scanner is a static textual-containment
   check (see its own header comment) and can't verify a call site actually
   passes the *right* user-facing detail, only that *some* logging call
   exists. Manually verify (or chromote-verify) that deleting a plain
   provision actually produces a sensible, readable Actividad entry, not
   just "some log_action fired."
4. Extend `tests/test_pasivos_audit_visibility.R` (Stage 5's own test file)
   with the specific gap you find and fix, rather than creating a
   disconnected new file — it already has the right synthetic-fixture
   pattern for testing `pasivos_log_audit()` in isolation.

---

## Issue E (Mouse's #4, part 2) — Cross-session update takes 10+ seconds with a bad "whitewash," and cross-client isolation of the sync mechanism needs to be proven, not assumed

**Mouse's words:** *"changing a provision or item from one date to another
does update live and does log, but if I edit on window A, B gets whitewashed
for like over 10 seconds and then it updates correctly. Slow af... it won't
work in prod. Also verify this does NOT happen whatsoever cross Clients (if
Hopdesk employee is changing stuff in their Hopdesk folder, it should NOT
interrupt anything on anyone's window ideally, but ESPECIALLY CRITICAL CROSS
CLIENTS ... otherwise this already decently large problem will be
catastrophical with more and more clients."*

This is the highest-risk, highest-uncertainty item in this document — treat
it with the most care, and do it last, after Issues A-D are done and the
regression suite is still green (a clean baseline makes it much easier to
tell whether something you do here actually changes behavior). **This item
overlaps directly with `STAGE_8_ISOLATION_SWEEP.md`, which was written and
never executed.** You are not starting from zero — you're finally exercising
a concern the team already flagged and is now getting concrete, hands-on
evidence for.

**Two separate sub-problems. Do not conflate them — fix/verify separately:**

### E1 — Why does cross-session propagation take 10+ seconds, and does the "whitewash" itself indicate something worse than just slowness?

`R/sync_bus.R`'s poller runs `invalidateLater(poll_ms, session)` with
`poll_ms = 8000` (`app.R:1294`, `setup_sync_bus(session, shared, poll_ms = 8000, ...)`).
8 seconds is already the documented, deliberately-chosen worst case for
*picking up* a change — 10+ seconds to *finish* updating is either (a)
expected given poll timing (a change made right after a poll tick has to
wait almost the full 8s for the next one, plus render time — verify whether
Mouse's observed ~10s is just unlucky timing relative to a fixed 8s poll, in
which case there's nothing broken, just an interval Mouse might want
tightened — this was flagged as an open question in the master plan's own
"Open items needing Mouse's input" section: *"8-second cross-session poll
interval... acceptable, or tighten?"* — you may finally have your answer),
or (b) evidence of something slower than the poll interval itself: a slow
`.refresh_ctx_detail()` recomputation, a slow full-modal `showModal()`
re-render (the "whitewash" — this term refers to Shiny's full-container
`renderUI` replace pattern, flagged as a UX concern **at the very start of
this whole session's work**, before any of the stages in the master plan —
worth reading that original framing if you can find it in conversation
history/memory, though it may not be written down anywhere retrievable),
or (c) a genuine bug in the version-diffing logic causing a missed tick
(look closely at `bump_sync_version()`'s S3 stamp write and
`setup_sync_bus()`'s S3 stamp read for a race — the comment at
`R/sync_bus.R:74-84` about clearing `.s3_missing_cache` before every poll
implies this has bitten the team before).

Instrument first, don't guess: add temporary timing logs (`message()` with
`Sys.time()` deltas, removed before you commit, or kept behind a debug flag
if genuinely useful) around the poll tick, the loader call, and the
`.refresh_ctx_detail()`/`showModal()` re-render, and get real numbers before
deciding what to optimize. The team has already successfully applied a
"defer the expensive write, batch it" pattern twice in this codebase
(`save_moves()` in `ledger_module.R`, and `ef714a1`'s `log_action()` fix,
described above) — that pattern may or may not be the right shape here (it
fixed *write* latency; this is a *read/render* latency problem, which is
different), but it's worth knowing it exists as prior art.

### E2 — Prove sync_bus actually respects the client-isolation boundary; do not assume it does because the parameters exist

This is the part with real stakes if it's wrong. Read `R/sync_bus.R` in
full (it's short, ~150 lines) before doing anything else. Specific,
concrete findings from reading it, presented as facts about the code, not
speculation:

- `.SYNC_REGISTRY` and `.GlobalEnv$.sync_versions` are **process-global**
  state (`.SYNC_REGISTRY[[name]] <<- ...` at module load; `register_synced()`
  is called once per dataset **name**, inside `server()`, so every new
  session re-registers the same global entries — functionally idempotent
  since every session registers identical name→loader mappings, but
  confirms there is exactly one registry for the whole R process, not one
  per session or per client).
- `bump_sync_version(name, client_id = NULL)` increments **one integer per
  dataset name**, global to the process. The `client_id` parameter it
  accepts is used only to decide *where* to write the cross-process version
  stamp file in S3 (`.s3_key(S3_KEYS$sync_versions, client_id = ...)`) — it
  does **not** scope the version number itself per client. Any session
  calling `bump_sync_version("tags_db")`, regardless of which client's data
  it just changed, bumps the same global counter every other session is
  watching.
- Per ARCHITECTURE.md §3, this is explicitly **one shared deployment**, not
  one process per client — meaning, if that's genuinely how it's currently
  running in production (verify this — check the actual shinyapps.io
  deployment config/env vars, not just the doc's stated intent), multiple
  different clients' real end-users could be connected to the exact same R
  process this registry lives in, at the same time.
- The per-session reload path (`setup_sync_bus()`'s poller,
  `R/sync_bus.R:100-145`) calls `entry$loader()` with **zero arguments**
  for any session not currently in a staff "jump" context
  (`in_jump_context`, computed by comparing `active_client_rv()` — which is
  `effective_client_id` for every session type, not just staff, per
  `app.R:1295` — against `Sys.getenv("CLIENT_ID")`). Whether that bare,
  argument-less `entry$loader()` call resolves to the correct client for a
  given session depends entirely on what each individual loader function
  (`load_tags`, `load_moves`, etc.) does with its own `client_id = NULL`
  default — and in a genuinely shared, multi-tenant single process, a
  *process-level* env var cannot represent "which client is THIS session,"
  by definition, since env vars aren't per-request.

**What this means you need to verify, concretely, before concluding anything
(this is exactly what STAGE_8_ISOLATION_SWEEP.md's Part B argues manual
testing cannot catch — build the harness it describes, don't just eyeball
this):**
1. What does `Sys.getenv("CLIENT_ID")` actually resolve to in the real,
   currently-running production deployment? Is it blank/neutral, or does it
   happen to equal one specific real client's slug? This single fact
   determines whether the scenario below is "occasionally wastes a
   re-render" or "actively serves the wrong client's data."
2. Build (or reuse, if Stage 8 Part B ever gets built first/in parallel — 
   check before duplicating effort) a concurrent-session test harness that
   runs two simulated sessions in the same R process with two *different*
   resolved `effective_client_id()` values, has one of them mutate a synced
   dataset and call `bump_sync_version()`, and asserts: (a) the other
   session's reactiveVal, after its own poll tick fires, contains **its
   own** client's data, never the triggering session's, and (b) — separately,
   and this is the part Mouse is specifically asking about — whether the
   other session's UI/reactives were forced to recompute/re-render **at
   all**, even if the data it ends up with is correct. Both matter: (a) is a
   correctness/security bug if it fails, (b) is the "don't interrupt anyone
   outside their own folder" UX requirement Mouse stated as non-negotiable,
   and can fail even when (a) passes.
3. If you find the process-global version-bump is causing (b) — unnecessary
   cross-client re-renders even where the data itself stays correctly
   scoped — that's a real bug to fix (scope the version counters per
   client_id, or gate each session's poll-check to only care about datasets
   actually relevant to *its own* client_id, or something else you design —
   this is genuinely your call once you understand the mechanism, this
   document is not prescribing the fix). If you find (a) actually fails —
   a session ever receives another client's data — stop, do not try to
   quietly patch it as part of this stage; flag it back immediately as a
   security-severity finding, the same way Stage 8's own charter says to
   treat this bug family ("do not fix unrelated bugs you notice along the
   way — flag them back," but a live cross-tenant data leak is not
   "unrelated," it's exactly what this sub-issue is investigating — use
   judgment, but err toward flagging and pausing rather than shipping a fix
   for something this sensitive without Mouse's explicit sign-off on the
   approach).
4. Hopdesk staff specifically: Mouse's example was "a Hopdesk employee
   changing stuff in their Hopdesk folder" — trace through what
   `effective_client_id()` resolves to for a staff member sitting in their
   own home context (`hd-admin`, no active jump) versus jumped into a real
   client, and confirm which datasets a staff-home-context action can
   possibly bump versions for, and who else's session would react to that
   bump under the current (probably-global) versioning.

**Suggested test plan:** the concurrent-session harness above is the core
deliverable — same spirit as `STAGE_8_ISOLATION_SWEEP.md` Part B, and if that
stage's harness gets built (by you, as part of this, or separately), this
should reuse it rather than building a second one. Per that stage's own
instruction: **prove the harness has teeth** by temporarily (locally, never
committed) breaking client-scoping on purpose and confirming the harness
catches it, before trusting a passing result. State the E1 timing numbers
and the E2 isolation proof explicitly in your final report — "it's better
now" is not an acceptable substitute for "here are the before/after
measurements and the specific assertion that passed."

---

## Suggested order of work

1. **Issue A** (auth) — self-contained, well-understood, lowest risk. Good
   first stage to build momentum and prove out the branch/test/commit
   rhythm for the rest.
2. **Issue D** (Pasivos logging gaps) — same shape as work just completed in
   Stage 6, lowest architectural risk, good second stage.
3. **Issue B** (modal reopening) — diagnosis is strong but needs live
   verification; moderate risk since the same broken pattern may be
   reused elsewhere.
4. **Issue C** (Bancos papelera) — new data model work, moderate-to-large
   effort, but no deep architectural uncertainty.
5. **Issue E** (sync latency + cross-client isolation) — do this last. It's
   the least understood, the highest-stakes if wrong, and benefits from B
   being fixed first (removes a confounding variable when you're trying to
   figure out why cross-session updates feel slow/disruptive).

Each is its own branch commit (or a few, if a stage is large — Issue E in
particular should probably be multiple commits). Full regression suite green
after each. Update this document (or the attempt-log file) with what you
actually found and did, especially anywhere your findings disagreed with
this document's hypotheses — that correction record is exactly as valuable
as the fix itself for whoever reads this next.

## Cleanup (do this last, after all 5 issues are done)

- Remove any temporary debug logging/instrumentation added during
  investigation (Issue E's timing logs, any chromote scratch scripts) unless
  you have a specific reason to keep something behind a flag — say so
  explicitly if you do.
- `git status` should be clean on your branch (nothing stray, no forgotten
  scratch files committed).
- Re-run both full suites one final time and paste the final pass counts
  into your report.
- Confirm `tests/test_log_action_completeness_scan.R` still reports 0
  actionable gaps.

## Explicitly out of scope for this stage

- Any bug you notice that isn't one of A-E above. Note it, don't fix it —
  flag it back the same way this project's convention already handles
  found-but-deferred items (see the master plan's "Open items needing
  Mouse's input" section for the tone/format).
- Redesigning `sync_bus.R` or the auth mechanism wholesale. Every fix above
  should be the smallest change that actually closes the gap, preserving
  every existing, working piece of behavior (live cross-session sync
  itself, `keep_token`'s refresh-survival behavior, the modal live-update
  behavior, etc.) — this is a bug-fixing stage, not a rearchitecture.
- Running the FULL `STAGE_8_ISOLATION_SWEEP.md` sweep (Part A's full
  codebase-wide pattern audit) unless you judge it genuinely necessary to
  properly close Issue E. A scoped read of `sync_bus.R` plus the targeted
  concurrent-session harness is the expected minimum; if you find yourself
  wanting to expand into the full Stage 8 sweep, that's a legitimate call to
  make, but say so explicitly in your plan/report rather than silently
  ballooning scope.

---

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\saas_rebuild\STAGE_9_LIVE_UI_BUGS.md
in full before doing anything else, then also read
docs\saas_rebuild\ARCHITECTURE.md and docs\saas_rebuild\STAGE_8_ISOLATION_SWEEP.md
as it instructs. Run `git log --oneline -20` to see the current state of the
repo before you start — other sessions have been actively working on this
codebase and the doc's file:line references may have drifted.

Work through Issues A through E in the order the document suggests, on a
new branch (fix/stage9-live-ui-bugs, or similar). For each issue: verify the
document's findings/hypotheses against the actual current code before
building anything on top of them (they were researched carefully but are
not guaranteed still accurate given ongoing parallel work), write a real
fix, back it with real tests (the project's own bar is at least 50 tests
per stage, more if warranted — favor a chromote-driven live browser test for
anything UI/cross-session, not just static analysis), run the full existing
suites (tests/_run_confirmed_logic.R and tests/_run_saas.R) and confirm both
stay green, then commit.

For Issue E specifically (cross-session latency + cross-client isolation):
this is the highest-stakes item. Do not conclude the isolation boundary is
safe or unsafe without building the concurrent-session test harness the
document describes and proving it has real teeth (temporarily break
isolation on purpose, confirm the harness catches it, restore, confirm it
passes). If you find any evidence of an actual cross-client data leak (not
just an unnecessary re-render), stop and report it immediately rather than
quietly patching it — that needs an explicit decision, not a unilateral fix
buried in this stage's diff.

Whenever an approach you try turns out wrong or needs reverting, append what
happened to docs/saas_rebuild/STAGE_9_ATTEMPT_LOG.md (create it) — what you
tried, why, and specifically why it didn't work — so it's never blindly
retried later. Comment every non-obvious fix in the code itself, matching
this codebase's existing style (grep for "found 2026-" to see the expected
tone).

Do not fix anything outside the 5 issues in the document — note it and flag
it back instead. Do not merge to master yourself; push your branch and tell
Mouse it's ready for review. Clean up any temporary/debug files before your
final commit. When all 5 issues are done, report back per-issue: what you
found (confirming or correcting this document's hypotheses), what you built,
the before/after test evidence, and anything you flagged back rather than
fixed.
```
