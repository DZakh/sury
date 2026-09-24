# Playbooks

Copy the matched one into the todolist verbatim. Every playbook ends with the
loop, Finish and the reply from `SKILL.md`, except **Report**.

## Change

1. Read the subsystem. Delegate the reading to an Explore subagent when it
   spans more than a few files. Name the data shape the change is about.
2. Write the contract into the ledger: the specs that must exist or move, the
   fuzzers that must stay green, the metrics that must not regress.
3. Candidates. When `packages/sury/src` changes and more than one design is
   plausible, sketch 2-3 in a few lines each, then spawn one subagent per
   serious candidate with `isolation: "worktree"` to build it far enough for
   `pnpm spec check --write` to measure (bundle, generated code, perf above the
   noise floor). Spawn `sury-judge` in **pick** mode with the sketches and the
   numbers. `skip: <reason>` when only one design is plausible.
4. Build the chosen candidate through the `spec` skill: docs, `S.res`,
   `index.d.ts` and specs included.
5. The loop.

## Bug

1. Reproduce as a spec example (`spec` skill) and confirm the golden records
   the wrong answer. Commit it before the fix, so history shows red then green.
2. Minimize the repro, then list hypotheses and rule them out with evidence
   (instrument, bisect `git log`) until one mechanism survives. Name the
   question being answered in the wrong place.
3. Find the siblings: every other site that reads the same wrong answer.
4. Contract: the example corrected, the siblings' examples, the `knownBugs.ts`
   entry deleted (if one), the fuzzer gate green. If a fuzzer should have caught
   it, extend its grammar or a property (`fuzz` skill).
5. Change steps 3-4 for the fix itself.
6. The loop.

## Hillclimb

1. Baseline: the metric and its noise floor, in the ledger.
2. One hypothesis per iteration: change, measure, keep if above the floor and
   nothing else regressed, revert otherwise. Each kept win is its own commit.
3. Contract: the target, or a plateau after a pivot.
4. The loop.

## Report

For a review or a comparison. No edits.

1. Review: spawn `sury-judge` in **review** mode on the target diff, and read
   the full changed files yourself. Add at least one alternative design,
   measured where cheap.
2. Compare: read the other library's changelog and source, map each feature to
   the Sury spelling that does the same today (checked against `entry.ts` and
   `docs/`), and classify it: have, different spelling, missing, not wanted
   (which goal it conflicts with). Check `IDEAS.md` before proposing one.
3. Reply with findings ranked most severe first, or the ranked missing
   features as the example a user would write.
