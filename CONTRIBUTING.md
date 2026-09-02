# Contributing

When it comes to open source, there are different ways you can contribute, all of which are valuable. Here's few guidelines that should help you as you prepare your contribution.

## Initial steps

Before you start working on a contribution, create an issue describing what you want to build. It's possible someone else is already working on something similar, or perhaps there is a reason that feature isn't implemented. The maintainers will point you in the right direction.

## Development

The following steps will get you setup to contribute changes to this repo:

1. Fork this repo.
2. Clone your forked repo: `git clone git@github.com:{your_username}/sury.git`
3. Install [pnpm](https://pnpm.io/) if not available `npm i -g pnpm@9.0.5`
4. Run `pnpm i` to install dependencies.
5. Run `pnpm test` in `packages/sury` for tests (it builds the entry bundle and compiles the ReScript bindings first). Use `pnpm res` if you want the ReScript compiler in watch mode while editing `S.res`.

The implementation lives in `packages/sury/src/*.ts` (see `CLAUDE.md` for the module layout); `src/S.res` is a thin ReScript bindings module on top of the same runtime.

## Architecture

This section describes the internal architecture of Sury to help with understanding and contributing to the codebase.

### Core Concepts

#### Schema (internal type)

The internal representation of a type schema, containing:

- `tag`: Type identifier (e.g., `stringTag`, `objectTag`, `arrayTag`)
- `decoder`: Builder function for input validation (type checking)
- `encoder`: Builder function for converting from different schema types
- `parser`: Builder function for transformations after decoding (used by `S.shape`, `S.to`)
- `serializer`: Builder function for reverse transformations
- `inputRefiner`: User validations run on the typed input, before the decoder
- `refiner`: User validations run on the assembled output, after the decoder (`S.reverse` swaps `inputRefiner` ↔ `refiner`)
- `to`: Target schema for transformations (set by `S.shape`, `S.to`)
- `from`: Path array indicating where this value comes from in shaped schemas
- `properties`: For object schemas, a dict of field name to schema
- `items`: For array/tuple schemas, an array of item schemas

#### Builder

A builder is a plain function with signature `(input: Val) => Val`. The schema being built is available as `input.e` (`expected` — there is no separate self-schema parameter). Builders generate JavaScript code at compile time by manipulating `val` objects:

```ts
const myBuilder = (input: Val): Val =>
  // `input.e` is this schema; return the output val
  B_next(input, `someTransform(${input.v()})`, input.e, input.e);
```

Encoders take an extra `target` argument (the schema being coerced into): `(input: Val, target: Internal) => Val`.

#### Val (Value)

A compilation-time representation of a value being processed. Key fields:

- `inline`: The generated code expression (e.g., `i["foo"]`, `v0`)
- `var()`: Function to allocate/retrieve a variable name (use when value is referenced multiple times)
- `schema`: The schema of the current value
- `expected`: The schema we're trying to parse/convert into
- `prev`: Link to the previous val in the transform chain (walked by `merge`)
- `codeFromPrev`: Generated statements that produce this val from `prev`, including the `let` declaration of its own value. A non-empty `codeFromPrev` makes the val non-hoistable in `merge`, so a union discriminant can't be lifted above a `let` it reads.
- `hoistedDecls`: `let` declarations hoisted *onto this val* by a descendant whose own segment was already emitted (a field read on its parent, a loop accumulator before its `for`). Populated with `B.hoistDecl(owner, decl)` and emitted by `merge` right after this val's checks — no callback mutating an unrelated val.
- `finalized`: set by `merge` once a val's code is emitted; a late cached-bond materialization re-reads inline instead of hoisting onto it (#240)
- `checks`: `array<check>` of type-narrows and user refiners. A check whose `fail === B.failInvalidType` is a type-narrow that doubles as a union dispatch discriminant. (Invariant: absent iff no checks — never stored as `Some([])`.)
- `isOutput`: `Some(true)` once refiners have run; advanced decoders (object/array/tuple/union/recursive) set it themselves
- `global`: Shared compilation context containing:
  - `embeded`: Array of embedded values (functions, constants) accessible as `e[n]`
  - `varCounter`: Counter for generating unique variable names

### Compilation Flow

When a schema operation is compiled (e.g., `parseOrThrow`), `parse(val)` runs a
loop until the val is fully decoded (`isOutput` is `Some(true)` and there is no
further `.to`). Each iteration:

```
Input Schema
     │
     ▼
┌──────────────────────────────────────────────────────────────┐
│  parse(val) loop — one iteration                             │
│                                                              │
│  if async flag:                                              │
│     - continue the chain inside `.then(...)`                 │
│                                                              │
│  else if val.isOutput (decoded, may still have `.to`):       │
│     - follow `.to`: run `expected.parser` (custom decoder)   │
│       or `refine` onto `.to` (default encoder coercion)      │
│                                                              │
│  else (not yet decoded):                                     │
│     1. Encoder — if `schema !== expected` and an encoder     │
│        exists, coerce between schema types                   │
│     2. Decoder — otherwise narrow to the schema type         │
│        (e.g. `typeof === "string"`) and push `checks`        │
│     3. markOutput — for primitive decoders, apply            │
│        `inputRefiner`/`refiner` and set `isOutput`           │
│        (advanced decoders own this themselves)               │
└──────────────────────────────────────────────────────────────┘
     │
     ▼
Output Val (chain of `.prev` links)
     │
     ▼
B.merge(output) → JavaScript code string → wrapped into the operation function
```

### Code Generation Example

For `S.object(s => s.field("foo", S.string))` the generated parse function is:

```javascript
i => {
  typeof i === "object" && i || e[1](i); // object validation
  let v0 = i["foo"];                     // field access
  typeof v0 === "string" || e[0](v0);    // string validation
  return v0;                             // return parsed value
};
```

Checks emit as `cond || e[n](x);` (throw when the condition is false), not as
`if (!cond) {...}`. Where:

- `i` is the input argument
- `e` is the embedded values array (error throwers, transformers), accessed as `e[n]`
- `v0`, `v1`, etc. are allocated variables

### Key Functions

- `parse(val)`: Main compilation loop — encoder → decoder → markOutput → follow `.to`, until the val is fully decoded
- `B_merge(val, hoistCond?)`: Walks the `.prev` chain into a code string. With `hoistCond` (union codegen) it lifts type-narrow checks into a dispatch condition; a val with non-empty `codeFromPrev` stays non-hoistable so its `let` travels with the check
- `B_next(prev, code, schema, expected)`: Creates the next val one step down the transform chain
- `B_refine(val, schema?, checks?)`: Clones a val to attach `checks` while preserving the var-allocation link
- `B_hoistDecl(owner, decl)`: Attaches a `let` declaration to a still-open owner val (prev/parent/self) that dominates and outlives the materialized value, replacing the old `allocate` side-channel
- `B_markOutput(val, valInput)`: Applies `inputRefiner`/`refiner` and marks the val as output
- `B_embed(val, value)`: Embeds a runtime value (function, object) and returns a reference like `e[0]`

### Shaped Schemas (S.shape, S.object with definer)

Shaped schemas use a proxy-based approach to track how values are used:

1. During schema definition, field accesses are tracked via `proxifyShapedSchema`
2. Each accessed field gets `from` set to its path (e.g., `["foo"]` for `s.field("foo", ...)`)
3. During parsing, `shapedParser` traverses the target structure and maps values from input
4. During serialization, `shapedSerializer` builds an accumulator (`acc`) that maps output paths to input vals, then `getShapedSerializerOutput` reconstructs the original structure

## PPX

### With Dune

Make sure running the below commands in `packages/sury-ppx/src`.

1. Create a sandbox with opam

```
opam switch create sury-ppx 5.3.0
```

Or

```
opam switch set sury-ppx
```

2. Install dependencies

```
opam install . --deps-only
```

3. Build

```
dune build --watch
```

4. Test

Make sure running tests

```
(run compiler for lib)
npm run res
(run compiler for tests)
npm run test:res
(run tests in watch mode)
npm run test -- --watch
```

## Make comparison

For the cross-library comparison table in the README, bundle each library on
https://bundlejs.com/ with the recipes below.

`sury`

```ts
export * as S from "sury@11.0.0-rc.1";
```

```ts
import * as S from "sury@11.0.0-rc.1";

const schema = S.schema({
  number: S.number,
  negNumber: S.number,
  maxNumber: S.number,
  string: S.string,
  longString: S.string,
  boolean: S.boolean,
  deeplyNested: {
    foo: S.string,
    num: S.number,
    bool: S.boolean,
  },
});
S.parser(schema)(data);
```

valibot

```ts
export * as v from "valibot@1.4.2";
```

```ts
import * as v from "valibot@1.4.2";

const schema = v.object({
  number: v.number(),
  negNumber: v.number(),
  maxNumber: v.number(),
  string: v.string(),
  longString: v.string(),
  boolean: v.boolean(),
  deeplyNested: v.object({
    foo: v.string(),
    num: v.number(),
    bool: v.boolean(),
  }),
});
v.parse(schema, data);
```

zod

```ts
export * as z from "zod@4.4.3";
```

```ts
import * as z from "zod@4.4.3";

const schema = z.object({
  number: z.number(),
  negNumber: z.number(),
  maxNumber: z.number(),
  string: z.string(),
  longString: z.string(),
  boolean: z.boolean(),
  deeplyNested: z.object({
    foo: z.string(),
    num: z.number(),
    bool: z.boolean(),
  }),
});
schema.parse(data);
```

### TypeBox

```ts
export * from "@sinclair/typebox@0.34.52";
// Include Value for transforms support
export * from "@sinclair/typebox@0.34.52/value";
export * from "@sinclair/typebox@0.34.52/compiler";
```

```ts
import { Type } from "@sinclair/typebox@0.34.52";
import { TypeCompiler } from "@sinclair/typebox@0.34.52/compiler";

const schema = TypeCompiler.Compile(
  Type.Object({
    number: Type.Number(),
    negNumber: Type.Number(),
    maxNumber: Type.Number(),
    string: Type.String(),
    longString: Type.String(),
    boolean: Type.Boolean(),
    deeplyNested: Type.Object({
      foo: Type.String(),
      num: Type.Number(),
      bool: Type.Boolean(),
    }),
  })
);
if (!schema.Check(data)) {
  throw new Error(schema.Errors(data).First()?.message);
}
```

ArkType

```ts
export * from "arktype@2.2.3";
```

```ts
import { type } from "arktype@2.2.3";

const schema = type({
  number: "number",
  negNumber: "number",
  maxNumber: "number",
  string: "string",
  longString: "string",
  boolean: "boolean",
  deeplyNested: {
    foo: "string",
    num: "number",
    bool: "boolean",
  },
});
schema(data);
```

## Spec Harness Suggestions

A running list of strictness or author-guidance features the spec harness
(`packages/spec`, see the `spec` skill) could add. When working on Sury you hit a
case the harness *should* have caught or guided better — a missing check, a weak
error message, a strictness gap that let a bad spec through — add a bullet here
instead of silently working around it.

- `jsonSchema` snapshots one target (the default draft-07), so an emit that is
  dialect-gated — `contentSchema` is 2019-09+, OpenAPI 3.0 has no content
  keywords at all — has no golden for the targets it differs on, and lands in
  `S_toJSONSchema_target_test.res` instead. A `jsonSchema.targets` map, or a
  per-spec target override, would keep it with the schema it belongs to.
- An operation whose output holds a class instance (`S.uint8Array` decoding to
  `Uint8Array`, `S.blob`/`S.file` to a `Blob`/`File`) can't be specced: the
  golden writer raises "cannot represent a Uint8Array instance as spec source
  code", and an op has no way to opt out — `_skip` is accepted under `vs.zod`
  but crashes the run under `operations.<op>` (`Cannot convert undefined or null
  to object`). Either teach the writer a constructor call for the common typed
  arrays and binary containers, or make `_skip` legal on an operation with a
  reason. It costs a whole direction of the content axis: the `codec-*` specs
  for `S.uint8Array`, `S.base64`, `S.blob` and `S.file` carry codegen and error
  cases only, and `tests/content_test.ts` holds the values instead.
- An example's `error` is matched verbatim, so one raised by the *platform*
  rather than by Sury pins that engine's wording: `new Blob([Symbol()])` says
  "Cannot convert a Symbol value to a string" on Node 22 and "The argument
  'value' is invalid" on Node 24, and the golden passed locally while failing
  CI. Write such an example so the message is ours — an input whose own
  `toString` throws — or the check could compare only the error's constructor
  when the spec says the failure is the platform's.
- `ts.schema` has to evaluate, so a schema whose *construction* panics — every
  argument the public API rejects outright, including the `"pack"`/`"unpack"`
  pairs that don't name two readings — has no spec at all, only a
  `creationError` for the ones that survive construction and fail at the
  operation. `tests/content_test.ts` holds those. A `ts.constructionError`
  beside `creationError` would keep them with the schema they reject.
- `operations` names `parse`, `decode` and `encode` only, so `S.assertInput` and
  `S.inputValidator` have no golden anywhere. Both compile through the same builder chain
  under a different result target, and a change to that target's handling broke
  every `S.assertInput(…, S.json)` and `S.inputValidator(S.jsonString)(…)` call with the whole
  suite green. An `assert` op block, even one holding just an expression and a
  pass/throw example, would have caught it; `tests/content_test.ts` holds it
  instead.
- A recursive schema the perf harness rebuilds per iteration can fail the
  `create+compile` phase where every other phase measures it fine —
  `specs/recursive-proto-name.yaml` raises "Cannot read properties of undefined
  (reading 'c')" there, and did before the fix that spec exists for, so it is
  the phase and not the schema. Rebuilding one inside an object schema fails the
  same way (`loopInput.e.decoder is not a function`), which is why the reserved
  names are two top-level specs rather than one object holding both. `recursive`
  keeps its defs in `globalConfig.d` for the duration of a build, so a harness
  that builds many schemas in one process is the thing that would see it.
- A golden containing a control character is written as a plain scalar, so
  `specs/ipv4.yaml` carries a literal tab and `specs/uri-template.yaml` a literal
  DEL and C1 byte, all of which the `yaml` package round-trips but PyYAML and
  yamllint reject outright. The set is exactly what `JSON.stringify` leaves raw:
  newlines and tabs it escapes are fine, DEL and the C1 block are not. Since the
  specs are published as documentation, the writer should quote or escape any
  scalar holding a control character. Reference-suite coverage is kept rather
  than trimmed to dodge this — the defect is in the writer.
- JSON Schema round trips now snapshot the type inferred by
  `S.fromJSONSchema(S.inputJSONSchema(schema))`, but not that second schema's type
  instantiation cost. Tracking the cost would catch regressions in the
  `FromJSONSchema` conditional type, at the expense of making every spec own a
  second TypeScript performance budget.
- `--perf` spawns one child process per target, and a target is one
  (spec, op, accept/reject) whose batch iterates every example of that outcome.
  It used to be one target per *example*, which made the job scale with
  coverage rather than with the library: measured on a 4-core box, 2511 targets
  against the current suite's 1480, and 3m34s wall / 7m39s CPU against 2m16s /
  4m49s. About 0.2s of CPU each, roughly 46ms of process startup (32ms node,
  14ms importing both bundles) and the rest batch time (20 warmup batches plus
  8 blocks × 2 rounds × 8 batches at 500µs).
  Aggregate rather than sample, if this is ever revisited. No rule picks a
  representative example well: the first is within 5% of its group's cheapest
  66% of the time, and the longest input is the priciest only 42% of the time,
  so "first accepted, first rejected" would measure a systematic best case. The
  costs of aggregating were measured instead — the megamorphic call site the
  loop creates costs 1.05x or less in 56% of groups and 1.20x or less in 77%,
  and lands on both sides of the ratio; a 2x regression on a group's cheapest
  example still moves the aggregate 33% at the median.
  What it does lose is the 25 of 353 groups whose internal spread is enormous:
  `union-large-planner` runs 5ns against 12µs, so its cheap member is 0.02% of
  the aggregate and a regression there is invisible. Splitting those back out
  is the fix, not electing a representative for them.
  Two things not to reach for. Batching targets into one process would save that
  46ms but give up the fresh heap per target the design deliberately buys. And
  raising the screening parallelism trades away exactly what the job is for —
  contention widens intervals, which hides regressions rather than inventing
  them.
- `ciRank` returns -1 below six blocks, and `conservativePct` then reports 0 for
  every target — so dropping `BLOCKS` under 6 to save time does not weaken the
  report, it silently empties it. Worth an assert next to the constant, since
  the failure looks exactly like "no regressions".
- Example values are recorded as source text, so an operation returning a class
  instance can only be snapshotted if the serializer knows that class. `Date`,
  `URL`, `RegExp`, `Map` and `Set` round-trip; anything else still fails with
  "cannot represent a … instance as spec source code", and the failure is
  recorded *as the example's golden* — so a passing operation is pinned as an
  error and reads like real behavior. Failing the check outright would be
  better than writing a golden the harness knows is a lie.
- A `URL` example is rendered from its `.href`, which makes the golden depend on
  the runtime's WHATWG parser rather than on Sury. `new URL("http://ex.com/a^b")`
  keeps the caret on Node 22 and normalizes it to `%5E` on the pinned Node 24, so
  the same spec is canonical on one and not the other, and the canonical-form
  test fails in CI with no Sury change behind it. Rendering the source string the
  example was written with — rather than the parsed value's serialization — would
  keep the golden about the schema. Until then a `URL` example silently pins
  runtime behavior, and the `engines` pin is the only thing keeping it honest.
- A codec spec is named `codec-<from>-<to>`, so `codec` is a prefix and never a
  suffix. Nothing enforces it — `url-codec.yaml` sat the other way round until
  it was renamed — and the id is what orders the specs directory, so the
  convention is only worth having if the linter holds it.
- A spec whose `operations` block omits an op the schema supports crashes the
  linter with `TypeError: Cannot read properties of undefined (reading
  'examples')` instead of naming the missing block. Hand-writing a spec rather
  than scaffolding it with `spec new` is the way in.
- No operation dimension for JSON-target conversions (`.to(S.json)` / `.to(S.jsonString)`), so bugs like #311 (nested optional fields failing to encode) can't be captured as spec examples — their repros live in `tests/` instead.
- A schema-creation error (a `panic` thrown while `ts.schema` evaluates) can't be a golden: `checkSpec` reports "ts.schema did not evaluate" instead of recording the message, so every rejection Sury raises at construction (e.g. a custom codec on a target that already converts) is pinned in `tests/` instead.
- `ts.schema` is TypeScript, so a surface with no TS spelling can't be specced at all. The ReScript codec seam (`~custom`, which reaches `S.to` as `{decodeToOutput, encodeFromOutput}`) is deliberately absent from `index.d.ts`, so its goldens — including the refiner-anchoring regression — live in `tests/S_to_custom_test.res`.
- Async operations have no spec dimension for the *encode* direction, so an async encode codec's success path is only visible as the sync op's `creationError` golden; the resolved value lives in `tests/`.
- Example results are serialized back to spec source, so a value keyed by a *non-registry* symbol can't be recorded ("cannot represent a non-registry symbol (use Symbol.for(key)) as spec source code"). A registry symbol round-trips as a computed key. `S.record`'s unvalidated symbol-keyed values are pinned in `tests/` instead of `specs/record.yaml`, where the rest of that gap lives.
- Scenario measurements can be bimodal across child processes: the identical `encoder-lookup` build measured "unchanged" and "−44%" against the same baseline in back-to-back runs, each individually printed as `confirmed`. The screening/rounds design averages within a process but can't see a whole process landing in a different JIT state (IC/feedback shapes settle per child, then every block agrees with itself). Until runs repeat the *process* (not just the rounds) and require agreement across them, treat any single scenario delta on a shared-arity path as one sample, not a verdict.

## License

By contributing your code to the rescript-schema GitHub repository, you agree to license your contribution under the MIT license.
