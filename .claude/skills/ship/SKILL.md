---
name: ship
description: Opt-in autonomous workflow, run as /ship <task>. Drives a change, bug or perf task in the Sury repo to production ready without asking, then pushes and reports the decision with measured alternatives.
disable-model-invocation: true
---

# Ship

Nobody watches the loop. Decide, build, prove, and hand back a result to react
to, never a question. `AskUserQuestion` only before something irreversible;
nothing in a playbook is.

## Rules

- Copy the playbook's steps into the todolist first. A step you don't do stays
  as `skip: <reason>`.
- A fork an experiment can settle is settled by running it. A taste call follows
  `taste.md`.
- Evidence is pasted output: a command's result, a golden that moved, the
  metric summary. "It compiles" is not evidence.
- Reproduce before fixing. Revert a change the evidence doesn't back.
- Delete before adding. A new concept must beat reusing an existing one, in
  numbers.
- Review every subagent's diff and write your own summary. Retry with a fresh
  agent, not a resume.
- Bulk reading and candidate builds go to subagents.

## Playbooks

**Answer** (a question, "should we"): reply from code, git history or a quick
probe. No ledger, no loop.

**Change** (new behaviour, refactor):
1. Read the subsystem; name the data shape.
2. Write the contract: specs that must exist or move, fuzzers that stay green,
   metrics that must not regress.
3. Candidates, when `packages/sury/src` changes and several designs are
   plausible: a subagent per candidate with `isolation: "worktree"`, built far
   enough for `pnpm spec check --write --perf=skip` to measure, and
   `--perf=only <ids>` for the specs it reaches. Pass `sury-judge` (pick mode)
   each sketch, worktree path and numbers (bundle gz, codegen, perf above the
   floor, lines +/-).
4. Build it through the `spec` skill, with docs, `S.res` and `index.d.ts`.
5. Loop.

**Bug** (issue, known bug, finding, red CI; one run and branch per root cause):
1. Reproduce as a spec example, confirm the golden records the wrong answer,
   and commit it before the fix.
2. Minimize, then rule out hypotheses with evidence until one mechanism is left.
   Name the question being answered in the wrong place.
3. Find every other site reading the same wrong answer.
4. Contract: examples corrected, `knownBugs.ts` entry gone (if one), fuzzer
   green, and the fuzzer extended if it should have caught this.
5. Change steps 3-5.

**Hillclimb** (faster/smaller): baseline and noise floor in the ledger, timed
with `pnpm spec check --perf=only <ids>` on the target's specs; one
hypothesis per iteration, kept as its own commit only above the floor with no
other regression. Contract: the target, or a plateau after a pivot.

**Report** (review a diff, compare a library): `sury-judge` review plus your own
read and an alternative, measured where cheap. Or read the other library's
changelog and source and map each feature to today's Sury spelling (checked
against `entry.ts` and `docs/`): have, different spelling, missing, not wanted.
Check `IDEAS.md`. No edits, no Loop, no Finish. Reply with findings ranked
most severe first, or missing features ranked, each as the example a user
would write.

Change, Bug and Hillclimb end with Loop, Finish and Reply.

## Ledger

`.ship/<branch>.md`, gitignored. Written before code, it survives compaction:

```md
## Contract
- [ ] <predicate a command or golden can check>
## Decisions
| # | decision | why | evidence | result |
```

Never relax the contract to finish. Log forks, measurements, accepted and
rejected findings, reverts.

## Loop

1. `pnpm verify --fast`. Red is the next thing to fix. Between rounds, check
   only the specs the change reaches: `pnpm spec check --write --perf=skip
   <ids>` takes seconds where all of them take minutes.
2. `sury-judge` review of the diff against `origin/main`. Fix blocking findings,
   each as a spec example first; log the rest with a reason.
3. `comment-sicko` on the diff; apply what you accept.
4. Deslop: one-caller helpers, defensive checks on trusted paths, `as any`, dead
   flags, anything off the surrounding file's dialect. Keep a cleanup only if
   `pnpm spec check --write --perf=skip <ids>` shows no regression.
5. Reflect: try the next deletion or simplification and measure it.

Exit when the contract holds and a whole round changed nothing. After two rounds
without progress, pivot once; if that stalls, stop and report the dead end.
Stop after round 8 regardless.

## Finish

1. `git fetch origin main && git merge origin/main`. Regenerate, never
   hand-merge: `pnpm spec check --write --perf=skip` (CI's `Performance (PR)`
   job times every target and posts the report on the PR),
   `pnpm benchmarks --write`, `pnpm --filter=sury build`, `pnpm spec schema`.
2. `pnpm verify`. Name any red that isn't this change's.
3. Commit, push to the session branch. Never open a PR.
4. If the branch has a PR, replace the body between `<!-- ship -->` and
   `<!-- /ship -->` (append them if missing), leaving the rest, with `##`
   sections: What changes for you (plain words, a before/after example),
   Impact (breaking and how to restore; speed/size a user notices), Decision
   (what was chosen and why, then the reply's table), Details (cause, what moved, tests, what verify could
   not run).

## Reply

Plain markdown for someone reading on a phone, never a code block around the
summary. Lead with what they notice first:

- **What changes for you**: the schema they write and what it reads back,
  before and after.
- **Why it happened**: one or two sentences, in their terms.
- **What else moved**: error messages, speed, size, a spec whose schema
  changed. Skip what nobody would notice.
- Name each principle next to the decision it changed.

Close with a short status, one fact per line:

**Done**: `pnpm verify` green (full), nothing blocking, 3 rounds. (Or
**Stopped**, and why.)
**Chosen** (confidence high|low): the approach in one sentence.

When more than one candidate was measured, add a table. Name each row by what
it does, not a letter alone, mark the one built, and write each cell as its
meaning ("+8 B gzip", "adds a try/catch on the valid path"). End with: Reply
"switch to B" to rework from B.

Confidence is low when goals 1-2 were close or the public API changed. "switch
to X" reruns Change from step 4 with X, keeping the ledger.
