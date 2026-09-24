---
name: ship
description: The one workflow for every prompt in the Sury repo. Triages the prompt, and for a change or bug drives it to production ready on its own - contract, measured candidates, verify, fresh-context review, cleanup - then pushes and reports the decision it took with the alternatives and their numbers. Use for every prompt here; it answers plain questions without the loop.
---

# Ship

You own the task end to end. Nobody is watching the loop: decide, build, prove,
and hand back a result to react to, never a question to answer.

## Non-negotiables

- **Todolist first.** Copy the matched playbook's steps from `playbooks.md`
  into the todolist verbatim before reasoning about the task. A step you don't
  do stays in the list as `skip: <reason>`. A silent skip is not allowed.
- **Never block on the human.** Everything here is reversible. A fork that an
  experiment can settle (behaviour, codegen, bundle, perf) is settled by
  running it. A taste call is made by `taste.md`. Neither is asked.
  `AskUserQuestion` is only for an irreversible action, and there are none in a
  playbook.
- **Prove it works.** "It compiles" and "tests pass" are not evidence on their
  own. Evidence is command output, a spec golden that moved, a metric summary.
  Paste it; don't paraphrase it.
- **Fix root causes.** Reproduce first. Every shipped line traces to evidence.
  A change that "might help" is a hypothesis: revert it when evidence doesn't
  back it.
- **Subtract before you add.** Delete dead weight first, then build on the
  simpler base. A new concept (field, flag, cache, parallel walk) has to beat
  the version that reuses what the core already has, in numbers.
- **You own every subagent's work.** Review its diff and write your own
  summary. For a second attempt spawn a fresh agent with the whole scope; don't
  chain resumes.
- **Guard the context window.** Bulk reading and candidate builds go to
  subagents; the main thread keeps summaries and the ledger.

## Triage

| prompt | playbook |
|---|---|
| a question, "how/why does X work", "should we" | **Answer**: reply from evidence (code, git history, a quick probe). No ledger, no loop. |
| new or changed behaviour, a refactor | **Change** |
| an issue, a bug, a known bug, a finding, red CI | **Bug** (several root causes: one Bug run each, one branch each) |
| "make X faster/smaller" | **Hillclimb** |
| review a branch/PR, compare with another library | **Report** |

## The ledger

`.ship/<branch>.md` (gitignored). It is what makes the run resumable after
compaction or in a new session, and what the final reply is built from.

```md
# <task>
## Contract
- [ ] <predicate, checkable by a command or a golden>
## Decisions
| # | decision | why | evidence | result |
```

The contract is written before the first line of code and never relaxed to
declare victory. Log decision points, not actions: a fork chosen, a candidate
measured, a finding accepted or rejected, a revert and its trigger.

## The loop

Each round, in order:

1. `pnpm verify --fast`. Red is the next thing to fix.
2. Review: spawn the `sury-judge` agent in **review** mode on the diff against
   `origin/main`. Blocking findings (architecture, correctness) are fixed this
   round, each as a spec example first. Log the rest as rejected with a reason.
3. Comments: spawn `comment-sicko` on the diff; apply what you accept.
4. Deslop the diff yourself: one-caller helpers, defensive checks on trusted
   paths, `as any`, dead flags, anything off the surrounding file's dialect.
   Keep a cleanup only if `pnpm spec check --write` shows no regression.
5. Reflect: what else could be deleted or simplified? Try it, measure it.

**Exit** when every contract box is ticked with evidence **and** a whole round
produced no accepted change. **Plateau** (two rounds with no contract progress):
pivot the approach once, logged. If the pivot stalls too, stop and report a dead
end with what you tried. Past round 8, stop and report regardless.

## Finish

1. `git fetch origin main && git merge origin/main`. Regenerate generated files
   on the merged tree, never hand-merge them: `pnpm spec check --write`,
   `pnpm benchmarks --write`, `pnpm --filter=sury build` (for `*.res.mjs`),
   `pnpm spec schema`.
2. `pnpm verify` (full). Everything green, or each red named with why it isn't
   this change's.
3. Commit and push to the session branch. Never open a PR.
4. If a PR already exists for the branch, rewrite the part of its body between
   `<!-- ship -->` and `<!-- /ship -->` (append the pair if absent; leave
   everything outside it alone) with the PR body below.

## PR body

Plain language first, detail last. No em dash.

```md
<!-- ship -->
## What changes for you
Two or three sentences a Sury user understands without reading code, then the
before/after example they would write.

## Impact
- **Breaking:** yes/no, and the spelling that restores the old behaviour.
- **Speed / size:** the numbers a user would notice, or "none".

## Decision
What was chosen and why, then the alternatives table from the reply.

## Details
Cause (for a fix), what moved in the core, tests that pin it, what `pnpm verify`
ran and what it could not.
<!-- /ship -->
```

## The reply

Short declarative sentences. Who the change is for and what they notice comes
before any implementation detail. Name each principle that changed a decision
and the choice it changed. Then:

```
Done: pnpm verify green (full), 0 blocking findings, 3 rounds.   [or: Stopped: <dead end>]

Decision (confidence: high|low): <chosen approach>
| | approach | bundle gz | codegen | perf | judge |
| A (built) | ... |
| B | ... |
Reply "switch to B" to rework from B.
```

Confidence is low when the pick was close on goals 1-2 or changes the public
API; say which. "switch to X" in a later prompt restarts the Change playbook
from step 4 with X as the base, keeping the ledger.
