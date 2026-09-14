# Handoff: delete retired dead code in R/bancos_module.R

Status: approved by the user — this is authorization to delete, not just audit.

## What this is

`R/bancos_module.R` (currently 3,966 lines) contains two large blocks of retired
functionality, each guarded by `if (FALSE) { ... }` so the code never executes,
plus a few smaller fragments referencing the same two retirements. They were
deliberately preserved instead of deleted at the time (git history already
holds the full pre-retirement version if it's ever needed), but nobody has
since decided to actually remove them, and they make up close to a quarter of
the file's bulk.

## Where they are (verify current line numbers yourself — this file changes)

As of commit `eb2d058`:

- `VINCULAR_RETIRED` ("Vincular" duplicate-merge feature):
  - CSS comment block: lines ~405-429
  - Reactive state stub: lines ~1533-1537 (`if (FALSE) { ... }`)
  - Main modal/server block: lines ~1661-2143 (`if (FALSE) { ... }`, ~483 lines)
  - A few explanatory comments pointing at the above: ~655-662
- `CONCILIAR_REMOVED` ("Sugerencias"/reconciliation feature):
  - CSS comment block: lines ~430-435
  - `sug_scoped_data` stub: lines ~967-982 (`if (FALSE) { ... }`)
  - Reactive state stub: lines ~1538-1544 (`if (FALSE) { ... }`)
  - Main modal/server block: lines ~2334-2713 (`if (FALSE) { ... }`, ~380 lines)

Find them with:
```
grep -n "VINCULAR_RETIRED\|CONCILIAR_REMOVED\|if *( *FALSE *)" R/bancos_module.R
```

## What to do

1. Re-locate every block above by its markers (don't trust the line numbers
   literally — confirm with the grep first, in case anything shifted).
2. Confirm each block really is unreachable: it's inside `if (FALSE) { ... }`
   or is a comment/CSS block with no live counterpart elsewhere referencing
   it. Grep the rest of the codebase for anything that calls into these
   blocks by name (e.g. any UI element ID or reactive name defined only
   inside them) — there shouldn't be any since the file itself calls them
   "not active," but confirm rather than assume.
3. Delete all of it: the `if (FALSE) { ... }` bodies and their wrapping
   conditional, the CSS comment blocks, and the small pointer-comments that
   only exist to explain the retired blocks (like the ones at ~655-662).
   Leave everything else in the file untouched — this is a pure deletion
   pass, not a refactor.
4. Run the existing test suite (whatever runner this repo uses for
   `R/bancos_module.R` — check `tests/` for `_run_*.R` files and the CI
   workflow) and confirm nothing regresses. If any test references the
   retired blocks by name, that test is itself dead and should be removed
   too, but flag that to the user rather than assuming.
5. Commit the change with a normal commit message describing what was
   removed and why (retired, unreferenced, preserved in git history).
6. Delete this handoff file (`HANDOFF_BANCOS_DEADCODE_CLEANUP.md`) once the
   above is done and committed.

## Explicitly out of scope

Nothing else in `R/bancos_module.R` or elsewhere. Don't touch active code,
don't "clean up while you're in there," don't chase unrelated findings — if
you notice something else that looks wrong, mention it in your final summary
instead of fixing it.
