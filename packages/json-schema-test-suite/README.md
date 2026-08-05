# json-schema-test-suite

Runs the official [JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite)
against `S.fromJSONSchema` and holds the score to a committed golden, so a
change in JSON Schema coverage shows up as a reviewable diff.

```bash
pnpm compliance                                  # check against goldens/ (what CI runs)
pnpm compliance --update                         # re-baseline after a change
pnpm compliance report draft2020-12              # per-file breakdown
pnpm compliance report draft7 --failures         # every failing test id
pnpm compliance report draft7 --divergent        # where S.is disagrees with S.parser
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
suite's `valid`. A schema that throws at conversion or compile time marks its
whole case as `errored`.

`S.is` is scored over the same corpus in parallel. It is the semantically
correct operation — JSON Schema is assertion-only — but it currently rejects
everything except `null` for `S.json`, which `fromJSONSchema` emits for `{}`,
`true`, and unrecognised keywords, so it scores far lower. The two operations
disagreeing is always a Sury bug rather than a JSON Schema gap, which makes
that delta a standing bug detector; the count is tracked in each golden and
the ids are available via `report --divergent`. Once the gap closes, `S.is`
becomes the canonical operation.

## Goldens

`goldens/<dialect>.json` records a summary plus the sorted ids of every failing
test and errored case. Only failures are listed, so a diff reads directly:
removed lines are newly passing, added lines are regressions. `check` fails on
drift in **either** direction — an improvement is supposed to land its golden
update in the same PR.

Goldens cover the required tests only. `optional/` (format assertion, bignum,
content encoding) is exploratory and deliberately unsnapshotted, because
whether Sury should claim spec-level `format` assertion is an open design
question rather than a bug.

## What the score is measuring

Sury is not a JSON Schema validator; the suite is being used to measure how
faithfully `S.fromJSONSchema` reproduces JSON Schema semantics. Two structural
gaps dominate the current number:

- **Under-validation** (the large majority of failures) — keywords
  `fromJSONSchema` doesn't implement yet are silently ignored, so invalid data
  is accepted. `$ref`/`$defs`, `patternProperties`, `uniqueItems`,
  `multipleOf`, `propertyNames`, `dependentRequired`, `min`/`maxProperties`,
  `contains`, `unevaluated*`, and `additionalProperties` as a schema are all in
  this bucket.
- **Over-strictness** — JSON Schema keywords are type-conditional assertions
  (`{"maxLength": 2}` must accept `100`; `{"properties": {…}}` must accept `5`;
  `{}` accepts anything), while `fromJSONSchema` builds typed schemas that
  reject the non-applicable type outright.

The second is a design decision rather than a defect: either `fromJSONSchema`
becomes faithful, or the supported subset gets documented and those tests stay
red on purpose. The goldens are a measurement, not a target — nothing here
asserts that 100% is the goal.
