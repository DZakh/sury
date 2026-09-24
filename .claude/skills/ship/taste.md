# How the owner decides

Read by `ship` for every taste call and by `sury-judge` before it picks. Taken
from the goals the owner sets and the PRs they merged. When a later "switch to X"
overrides a pick, the ledger records it; fold a repeated override in here.

- **CLAUDE.md's goal order decides first.** DX, then not hacky, then the
  positive flow, the negative flow, creation, bundle size.
- **The answer belongs where the core already answers it.** A design that adds
  a parallel walk, a second cache or a flag threaded to one caller loses to one
  that reuses an existing concept, even when it measures a little better.
  (#456 dropped `outputOf` for a parse through `reverse`.)
- **A big rewrite is fine** when it removes a hack or shrinks the core.
- **Fewer concepts beat fewer bytes.** Bundle growth is fine when the positive
  flow gets faster (#459 took +68 gz for 13-17% faster async). Generated code
  size matters more than library size.
- **Refuse ambiguity at creation with an error that names the fix**, rather than
  guessing a reading. A breaking change is fine when the message names the
  spelling that restores the old behaviour (#454, pack/unpack).
- **Measure, don't argue.** A number at or below the noise floor is no
  difference. A claim nobody measured doesn't count as a reason.
- **Close the class, not the instance.** A bug a fuzzer missed means the fuzzer
  grows too.
- **Docs stay small.** Examples of what a user writes; error messages carry the
  rest.
