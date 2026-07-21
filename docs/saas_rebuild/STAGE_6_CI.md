# Stage 6 — Continuous Integration (run the test suite on every push)

Branch off `master` (the checkpoint branch's job is done — `master` is now
the canonical base for every future stage; stop referencing
`saas-checkpoint-2026-07-15` in new work).

## Goal

A GitHub Actions workflow that runs the R test suite(s) automatically on
every push and reports pass/fail as a check on GitHub — so a regression is
caught the moment it happens, not the next time someone remembers to run
`Rscript tests/_run_saas.R` by hand.

## Scope — confirmed with Mouse

Run **every** test runner in `tests/`, not just the 362-test SaaS suite —
Mouse confirmed all suites currently pass. Before wiring each one into CI,
verify it is fully mocked (no live AWS/API credentials required) the same
way `tests/_run_saas.R` and `tests/test_forecasting_sources.R` already are
(both confirmed during this audit — `_run_saas.R`'s own header states "No
live S3 credentials needed," and the forecasting-source tests monkey-patch
`httr::GET` rather than hitting Banxico/FRED/Yahoo for real). If any runner
turns out to need real credentials or network access, **stop and report
back** rather than silently skipping it or wiring real secrets into CI —
that's a decision for Mouse, not an assumption to make.

Known runners to include (verify each, don't assume): `_run_saas.R`,
`_run_agenda.R`, `_run_cart.R`, `_run_forecasting.R`, `_run_s1.R`,
`_run_s2.R`, `_run_s3.R`, `_run_tag.R`.

**Report-only for now, not merge-blocking.** Mouse isn't using pull
requests as the normal workflow yet (stages have been merged directly) —
don't add branch protection rules requiring the check to pass. Revisit
this once PRs become the normal way work lands.

**Confirmed with Mouse: "report-only" must still surface enough detail to
troubleshoot without digging through raw logs.** A bare red ✗ with no
detail forces someone to go spelunking in the full Actions log to find out
what actually broke. Design the workflow so the *first thing anyone sees*
on a failed run is a short, specific summary — not just pass/fail counts.

## Design

`.github/workflows/test.yml`:

- Trigger: `on: push` (all branches) — this covers Mouse's direct-merge
  workflow, not just PRs.
- Runner: `ubuntu-latest`.
- Steps: checkout, set up R via `r-lib/actions/setup-r` (pin a specific R
  version close to what's used locally — `4.5.2` — rather than "latest,"
  so CI failures are never just "a newer R broke something unrelated"),
  install the package set below, run each verified-mockable test runner in
  sequence, fail the job if any runner reports a failure.
- Package list (no `DESCRIPTION`/`renv.lock` exists in this repo — derive
  from `R/global.R`'s `library()` calls plus test-only additions):
  `shiny, shinyjs, bslib, dplyr, tidyr, lubridate, stringr, scales, purrr,
  DT, shinyWidgets, aws.s3, httr, jsonlite, uuid, readxl, openxlsx,
  shinymanager, callr, openssl, tibble, stringi` — plus `bcrypt` once
  Stage 7 lands. Use `r-lib/actions/setup-r-dependencies` or a plain
  `install.packages()` step; either is fine, prefer whichever is simpler
  to keep working without a package manifest to sync.
- Each runner's `PASS`/`FAIL` line output should be visible in the Actions
  log as-is (they already print this) — no need to reformat, just make
  sure the job's exit code reflects failure when any runner reports one
  (several of these runners use `message()`/`cat()` for PASS/FAIL rather
  than `stop()` on failure — confirm how failure actually propagates as a
  process exit code for each; if a runner would print "FAIL" but still
  exit 0, wrap it so CI actually catches that, don't trust the printed
  text alone).
- **Failure detail, surfaced up front — not just an exit code.** Write a
  GitHub Actions step summary (`$GITHUB_STEP_SUMMARY`) for every runner:
  which runner it was, how many passed/failed, and — for any failing
  runner — the specific failing test descriptions/assertions it printed
  (these test scripts already name each check in their PASS/FAIL lines;
  capture and forward that text into the summary rather than only the
  final tally). This should read, at a glance from the run's summary page
  with zero scrolling through raw logs, "which test file, which specific
  check, what it expected vs. got." If a given runner's own output doesn't
  carry enough detail to say what actually failed (some may only print a
  final count), improve that runner's own failure messaging as part of
  this stage rather than leaving CI to report a bare "something in
  _run_X.R failed."

## Explicitly out of scope

- Branch protection / merge-blocking.
- Running the actual Shiny app itself, browser/integration tests, or
  anything that would need a real S3 bucket or SAP server — this stage is
  the existing R-level test suites only.
- A package manifest file (`DESCRIPTION`/`renv.lock`) — worth doing
  eventually for reproducibility, but not required to make CI work and not
  this stage's job.

## Test plan

- Push a small, deliberately trivial commit (e.g. a comment) and confirm
  the Action runs and reports success in the GitHub Actions tab.
- Temporarily and locally break one assertion in any test file (don't
  commit this), confirm the runner would report a failure, and confirm
  your workflow's exit-code handling would actually surface that as a
  failed CI check — this is the real proof the "report-only" check has
  teeth, not just green checkmarks by default.
- With that same deliberately-broken test, confirm the step summary names
  the specific broken check by its own description — not just "1 test
  failed" — before reverting the deliberate break. This is the real proof
  the detail requirement is met, not just the pass/fail signal.
- Confirm the workflow does NOT require any GitHub Actions secrets to be
  configured — if it does, stop, because that means one of the "verify
  it's mocked" checks above was wrong.

## Prompt for the implementing Claude Code session

```
Read C:\Users\luisr\Antiguedad_App\docs\saas_rebuild\STAGE_6_CI.md in full
before writing anything. Branch off master (not saas-checkpoint-2026-07-15
— that branch's job is done, master is now canonical), name the branch
stage-6-ci.

For each test runner in tests/, verify it is fully mocked (no live AWS/API
credentials needed) before including it in the workflow — check each
runner's own header/comments and its actual code, don't assume based on
this document's list. If any runner needs real credentials, stop and
report back rather than silently skipping it or adding secrets.

Create .github/workflows/test.yml per this document's Design section.
Report-only, no branch protection changes - but the report itself must
surface specific, actionable failure detail (which runner, which named
check, expected vs. got) via a GitHub Actions step summary, not just a
red/green result. If any test runner's own output doesn't carry enough
detail to identify what broke, improve that runner's failure messaging
too - don't ship a CI report that just says "something in _run_X.R
failed."

Verify the workflow actually catches a failure (per the Test Plan) before
declaring this done - a CI setup that always shows green regardless of
test outcome is worse than no CI at all. Confirm the step summary names
the specific broken check when you deliberately break something, per the
Test Plan.

When done, report back exactly which test runners were included/excluded
and why, confirm the workflow requires zero secrets, and show an example
of what the step summary looks like on a failing run. Do not merge to
master yourself.
```
