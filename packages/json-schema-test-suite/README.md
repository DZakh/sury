# json-schema-test-suite

Runs the official [JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite)
against `S.fromJSONSchema` and holds the score to a committed golden, so a
change in JSON Schema coverage shows up as a reviewable diff.

```bash
pnpm compliance                                  # check against goldens/ (what CI runs)
pnpm compliance --update                         # re-baseline after a change
pnpm compliance report draft2020-12              # per-file breakdown
pnpm compliance report draft7 --failures         # every failing test id
pnpm compliance report draft7 --divergent        # where S.inputValidator disagrees with S.parser
pnpm compliance report draft7 --mutated          # valid inputs changed by parsing
pnpm compliance report draft7 --optional         # include optional/ (formats, bignum, content)
```

## How it works

The suite is not an npm dependency — the `@json-schema-org/tests` mirror is
archived and lags upstream, and a git submodule would tax every clone and CI
checkout with `--recursive`. Instead `suite.ts` fetches the single commit
pinned in `suite-ref.json` into a gitignored `.suite/`. Bumping that commit is
a deliberate PR; regenerate the goldens in the same commit so the diff shows
what the new tests changed.

Each suite assertion is run as `S.fromJSONSchema(schema)` followed by
`S.parser(schema)(data)`, and a test passes when the parse outcome matches the
suite's `valid`. Every valid example that parses also has an output-identity
assertion: because JSON Schema only validates, parsing must return deeply equal
data. A schema that throws at conversion or compile time marks its whole case
as `errored`.

`S.inputValidator` is scored over the same corpus in parallel. The two operations
disagreeing is always a Sury bug rather than a JSON Schema gap, so that delta is
a standing bug detector; the count is tracked in each golden and the ids are
available via `report --divergent`.

## Goldens

`goldens/<dialect>.json` records a summary plus the sorted ids of every failing
test, errored case, false acceptance, and valid input whose output was changed.
The lists make a diff read directly: removed lines are newly passing, added
lines are regressions. `check` fails on drift in **either** direction — an
improvement is supposed to land its golden update in the same PR.

Goldens cover the required tests only. `optional/` (format assertion, bignum,
content encoding) is exploratory and deliberately unsnapshotted, because
whether Sury should claim spec-level `format` assertion is an open design
question rather than a bug.

## What the score is measuring

Sury is not a JSON Schema validator; the suite measures how faithfully
`S.fromJSONSchema` reproduces JSON Schema semantics. Unsupported assertion
keywords fail conversion instead of silently widening the schema. Remaining
conversion gaps are `unevaluatedProperties` / `unevaluatedItems`, and anything
that needs resource or dynamic scope.

Local JSON Pointer `$ref`s resolve, including recursive definitions. `$id`,
`$anchor`, `$dynamicRef`, a remote URI, or a URN remain outside the supported
reference model and generally fail conversion. draft2020-12 has one
non-conversion mismatch: a custom metaschema with no validation vocabulary
still validates, because Sury is a codec and does not honor `$vocabulary`.

The goldens are a measurement, not a target — nothing here asserts that 100%
coverage is the goal.
