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
- `dc` (decoder): Builder function for input validation (type checking)
- `en` (encoder): Builder function for converting from different schema types
- `pr` (parser): Builder function for transformations after decoding (used by `S.shape`, `S.to`)
- `sz` (serializer): Builder function for reverse transformations
- `ir` (inputRefiner): User validations run on the typed input, before the decoder
- `rf` (refiner): User validations run on the assembled output, after the decoder (`S.reverse` swaps `ir` ↔ `rf`)
- `to`: Target schema for transformations (set by `S.shape`, `S.to`)
- `from`: Path array indicating where this value comes from in shaped schemas
- `properties`: For object schemas, a dict of field name to schema
- `items`: For array/tuple schemas, an array of item schemas

#### Builder

A builder is a plain function with signature `(input: Val) => Val`. The schema being built is available as `input.e` (`expected` - there is no separate self-schema parameter). Builders generate JavaScript code at compile time by manipulating `val` objects:

```ts
const myBuilder = (input: Val): Val =>
  // `input.e` is this schema; return the output val
  B_next(input, `someTransform(${input.v()})`, input.e, input.e);
```

Encoders take an extra `target` argument (the schema being coerced into): `(input: Val, target: Internal) => Val`.

#### Val (Value)

A compilation-time representation of a value being processed. Key fields:

- `inline`: The generated code expression (e.g., `i.foo`, `v0`)
- `var()`: Function to allocate/retrieve a variable name (use when value is referenced multiple times)
- `schema`: The schema of the current value
- `expected`: The schema we're trying to parse/convert into
- `prev`: Link to the previous val in the transform chain (walked by `merge`)
- `codeFromPrev`: Generated statements that produce this val from `prev`, including the `let` declaration of its own value. A non-empty `codeFromPrev` makes the val non-hoistable in `merge`, so a union discriminant can't be lifted above a `let` it reads.
- `hoistedDecls`: `let` declarations hoisted *onto this val* by a descendant whose own segment was already emitted (a field read on its parent, a loop accumulator before its `for`). Populated with `B.hoistDecl(owner, decl)` and emitted by `merge` right after this val's checks - no callback mutating an unrelated val.
- `finalized`: set by `merge` once a val's code is emitted; a late cached-bond materialization re-reads inline instead of hoisting onto it (#240)
- `checks`: `array<check>` of type-narrows and user refiners. A check whose `fail === B.failInvalidType` is a type-narrow that doubles as a union dispatch discriminant. (Invariant: absent iff no checks - never stored as `Some([])`.)
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
│  parse(val) loop - one iteration                             │
│                                                              │
│  if async flag:                                              │
│     - continue the chain inside `.then(...)`                 │
│                                                              │
│  else if val.isOutput (decoded, may still have `.to`):       │
│     - follow `.to`: run `expected.parser` (custom decoder)   │
│       or `refine` onto `.to` (default encoder coercion)      │
│                                                              │
│  else (not yet decoded):                                     │
│     1. Encoder - if `schema !== expected` and an encoder     │
│        exists, coerce between schema types                   │
│     2. Decoder - otherwise narrow to the schema type         │
│        (e.g. `typeof === "string"`) and push `checks`        │
│     3. markOutput - for primitive decoders, apply            │
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
  let v0 = i.foo;                        // field access
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

- `parse(val)`: Main compilation loop - encoder → decoder → markOutput → follow `.to`, until the val is fully decoded
- `B_merge(val, hoistCond?)`: Walks the `.prev` chain into a code string. With `hoistCond` (union codegen) it lifts type-narrow checks into a dispatch condition; a val with non-empty `codeFromPrev` stays non-hoistable so its `let` travels with the check
- `B_next(prev, code, schema, expected)`: Creates the next val one step down the transform chain
- `B_refine(val, schema?, checks?)`: Clones a val to attach `checks` while preserving the var-allocation link
- `B_hoistDecl(owner, decl)`: Attaches a `let` declaration to a still-open owner val (prev/parent/self) that dominates and outlives the materialized value, replacing the old `allocate` side-channel
- `B_markOutput(val, valInput)`: Applies `ir`/`rf` and marks the val as output
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

`pnpm benchmarks --write` regenerates `docs/benchmarks/*.md`, which is where the
measured cross-library numbers live: bundle size, feature probes, timings and
conformance, for schemas, JSON encoding, JSON Schema and protobuf. CI runs the
check on every pull request and republishes the pages on every push to main, so
a number there is never older than the last merge. Reach for it first.

The recipes below are for the README's own snapshot table, which is bundled on
https://bundlejs.com/ so the figures are comparable with what that site reports.

A release is measured before it is on npm, so Sury's own cells come from the
local build instead: bundle the same recipe with esbuild (`bundle`, `minify`,
`treeShaking`, `format: esm`, `target: esnext`, `platform: browser`) and gzip
the output - that reproduces bundlejs within 0.6% on a published version, and
the total agrees with `specs/bundleSize.yaml`. Re-check against bundlejs once
the release is published.

`sury`

```ts
export * as S from "sury@11.0.0";
```

```ts
import * as S from "sury@11.0.0";

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
S.parseOrThrow(schema)(data);
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
case the harness *should* have caught or guided better - a missing check, a weak
error message, a strictness gap that let a bad spec through - add a bullet here
instead of silently working around it.

- An example's `error` is matched verbatim, and `errorConstructor` is the
  opt-out for a message that belongs to the platform rather than to Sury.
  Nothing points an author at it: the failure is a golden that passed locally
  and differs on the CI runner's Node, and the diff names the wording without
  saying whose it is. The class prefix a foreign throw now carries is the
  signal to key on - a mismatch on an `error` golden whose two sides share a
  class could say "this message is the platform's; record `errorConstructor`
  instead".

- `operations` has an `assert` and an `is` slot and no spec fills either, so
  `S.assertInputOrThrow` and `S.isInput` still have no golden anywhere. Both
  compile through the same builder chain under a different result target, and a
  change to that target's handling broke every `S.assertInputOrThrow(..., S.json)`
  and `S.isInput(S.jsonString)(...)` call with the whole suite green. One block,
  holding just an expression and a pass/throw example, would have caught it;
  `tests/content_test.ts` holds it instead.

- `operations` names one schema's `parse`/`decode`/`encode`, so a **pipeline**
  - `S.decodeOrThrow(a, b, c)`, the multi-schema form `docs/js-usage.md`
  documents - has no golden anywhere. It is not a niche path:
  `S.decodeOrThrow(base64, jsonString, string)` emits byte for byte what
  `S.base64.with(S.to, S.jsonString).with(S.to, S.string)` does, through a
  fold of its own (`compileChain`), and nothing pins that the two agree.
  A `ts.pipeline` beside `ts.schema`, taking the argument list, would cover it.

- `fuzz:union --ref=<commit>` reports 3 `acceptance` diffs on the pinned
  `issue-392` case even when the working tree *is* that commit, so the
  changelog cannot be read as a signal without running it on an unchanged tree
  first. The pinned case builds its member schemas with the working library and
  hands them to `baseline.union(...)`, so two library instances share one set of
  schema objects and each writes its own per-compile fields onto them. Building
  the members with each library separately, the way `diffsForUnion` does for the
  generated cases, would make the changelog trustworthy. The gate itself is
  unaffected - it only counts `acceptance`/`exception-kind` from the
  compiled-vs-reference run.

- There is no operation for source text: `S.toProtoOrThrow` returns a `.proto` file,
  so its output is snapshotted in `tests/S_toProto_test.ts` rather than a spec.
  A `proto` operation next to `jsonSchema` would keep it with the schema.

- `valueToCode` has no case for `ArrayBuffer`, so an example whose result is
  one (`S.arrayBuffer`, `S.uint8Array.with(S.to, S.arrayBuffer)` encode) can
  only be a rejecting input; the accepting ones live in
  `tests/S_arrayBuffer_test.ts`. `new Uint8Array([...]).buffer` would print it.

- `jsonSchema` snapshots one target (the default draft-07), so an emit that is
  dialect-gated - `contentSchema` is 2019-09+, OpenAPI 3.0 has no content
  keywords at all - has no golden for the targets it differs on, and lands in
  `S_inputJSONSchema_target_test.res` instead. A `jsonSchema.targets` map, or a
  per-spec target override, would keep it with the schema it belongs to.

- A spec for a *new* export is timed against a baseline that doesn't have it.
  The expression evaluates to `undefined` there, `S.parseOrThrow(undefined)` compiles
  to `noopOperation`, and the real validator is then reported as thousands of
  percent slower than a function that returns its input - PR #420 added 14
  formats and got 17 such rows, every one of them bogus. The harness already
  knows how to say `new:` (the same spec's `decode`/`encode` targets are listed
  that way), so `parse` could take the same path when the baseline expression
  is `undefined`, rather than comparing against a no-op. The accompanying
  `behavior changed - baseline accepted it, now rejected` lines have the same
  cause: a no-op accepts every input, valid or not.

- What decides whether a `divergence` exists at all is accept-or-reject, so an
  example both libraries accept records nothing, and a value difference there is
  invisible. Once one exists its `zod` answer is a golden like any other - a
  changed value is reported against the one it replaced - but an author who
  notices the difference first and records it is told `divergence.zod agrees
  with parse - remove it`. Comparing that answer against the example's own
  `output` is what would close it, and would turn every coercion difference into
  a finding; the reason it is not done today is noise, not principle, and now
  that the answers are recorded the size of that noise is measurable first.

- `divergence.ajv` gates only the direction that is a promise: a document that
  rejects what the parser accepts. The other direction is deliberately unasked,
  because JSON Schema is allowed to describe a wider set (a refinement has no
  keyword) - but that also means a document far wider than the parser, one that
  accepts everything, passes silently. A count of how many *rejected* examples
  each document turns away would rank the specs where it has drifted furthest
  without ever failing one.

- Only the default draft-07 emit is validated against the examples. The
  `draft-2020-12` and `openapi-3.0` blocks record just the fields that differ
  from it, so there is no whole document to hand a validator - reassembling one
  by merging the sparse block over the default would extend the same check to
  both dialects, and those are the targets a consumer is most likely to publish.

- The `vs` dimension names Zod alone. `checkZodExamples` reads it through
  Standard Schema (`~standard`), not a Zod API, so a `vs.valibot` or
  `vs.arktype` would need only the import line and a key in the format - the
  cross-check itself would work unchanged.

- Nothing measures which *branch* of a generated operation an example reaches.
  Every failure path calls an embedded function (`e[0](i)`), so wrapping the
  embed array with counters during a recompute would give per-operation branch
  coverage for free, and an uncovered failure site is exactly the check no
  example exercises.

- `checkAliases` compares `fn.toString()`, but a compiled operation also carries
  the `embedded` array every `e[k]` in that text indexes into. Two schemas whose
  code is identical and whose embeds differ (a different refinement closure, a
  different error message) read as equivalent.

## License

By contributing your code to the rescript-schema GitHub repository, you agree to license your contribution under the MIT license.
