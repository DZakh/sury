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
5. Run `pnpm res` to run ReScript compiler
6. Run `pnpm test` for tests or use Wallaby.js

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

A builder is a function with signature `(~input: val) => val`. The schema being built is available as `input.expected` (there is no `~selfSchema` parameter). Builders generate JavaScript code at compile time by manipulating `val` objects. They are created using `Builder.make`:

```rescript
let myBuilder = Builder.make((~input) => {
  // `input.expected` is this schema; return the output val
  input->B.next(`someTransform(${input.var()})`, ~schema=input.expected)
})
```

Encoders take an extra `~target` (the schema being coerced into) and are created with `Builder.encoder`:

```rescript
let myEncoder = Builder.encoder((~input, ~target) => { ... })
```

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
- `B.merge(val, ~hoistCond=?)`: Walks the `.prev` chain into a code string. With `~hoistCond` (union codegen) it lifts type-narrow checks into a dispatch condition; a val with non-empty `codeFromPrev` stays non-hoistable so its `let` travels with the check
- `B.next(prev, code, ~schema, ~expected=?)`: Creates the next val one step down the transform chain
- `B.refine(val, ~checks=?, ~schema=?, ~expected=?)`: Clones a val to attach `checks` while preserving the var-allocation link
- `B.hoistDecl(owner, decl)`: Attaches a `let` declaration to a still-open owner val (prev/parent/self) that dominates and outlives the materialized value, replacing the old `allocate` side-channel
- `B.markOutput(val, ~valInput)`: Applies `inputRefiner`/`refiner` and marks the val as output
- `B.embed(val, value)`: Embeds a runtime value (function, object) and returns a reference like `e[0]`

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

## Specs (AI-first test suite)

Alongside the hand-written `tests/*_test.res` / `tests/*_test.ts` files, Sury has a
declarative spec suite under `packages/sury/specs/`. It exists because Sury is
effectively a compiler: one schema is compiled into generated code, exported to
JSON Schema, and given a TypeScript type — and each of those outputs can regress
independently. The specs capture a schema's **whole contract** in one file so
those dimensions stay in sync, and the format is optimized to be **written and
maintained by machines** (an LLM or a script), reviewed by humans, and executed
by tooling.

### The idea

- **One file = one schema's full contract.** `specs/string.yaml` describes
  `S.string`: its type, its input/output JSON Schema, and — per operation
  (`parse` / `decode` / `encode`) — the generated code (`expression`) plus named
  input→output|error examples.
- **The spec is the source; tests are generated.** `pnpm spec gen` expands each
  spec into a committed, real Vitest file under `tests/generated/*.gen_test.ts`.
  So plain `pnpm test` and Wallaby run them with zero custom infrastructure, and
  a breaking API change fails the generated files at **compile time**.
- **Golden-master, never hand-written goldens.** You author the schema and
  example *inputs*; `pnpm spec update` runs the real schema and fills in every
  derivable answer — `expression`, `jsonSchema`, and each example's
  `output`/`error`. You never type a golden by hand.
- **Exhaustive by construction.** Every dimension and every operation is
  *required*. A dimension you don't assert must say so explicitly with
  `_skip: <reason>` (an enum like `parser-only`, `lossy`, `identity`, or
  `todo(#…)`). An omission is a bug; an unexplained skip is rejected. This is the
  key AI-safety property: a machine author cannot silently drop coverage —
  `grep -r 'todo' specs/` is the coverage backlog.
- **Closed world & canonical form.** The format is defined *as a Sury schema*
  (`scripts/spec/format.mjs`) which emits `specs/spec.schema.json` (the
  `$schema` editors and AI authors validate against). Unknown keys are rejected;
  `_`-prefixed keys are the reserved harness namespace. `pnpm spec fmt` rewrites
  specs to a byte-deterministic canonical form so diffs show only semantic change.

### Workflow

```
pnpm spec new <id>            # scaffold specs/<id>.yaml (every dimension = _skip: todo)
# …fill in schema.ts and example inputs…
pnpm spec update <id>         # run the schema, fill goldens, regenerate the test
pnpm test                     # generated test runs like any other (behavior + types)
pnpm spec:check               # CI gate — see below
```

`pnpm spec:check` is the fast pre-commit / CI gate (no ReScript build needed). It
verifies, for every spec: it validates against the format schema, every `_skip`
reason is well-formed, the file is in canonical form, the stored goldens match
what the live schema produces, and the generated test file is current (byte- and
sha-matched). Behavior itself is still asserted by `pnpm test` running the
generated files.

### Dimensions

| Dimension | Status | Source of truth |
| --- | --- | --- |
| `operations.<op>.expression` (codegen) | live | `.toString()` of `S.parser`/`S.decoder`/`S.encoder` |
| `operations.<op>.examples` (input→output\|error) | live | executing the operation |
| `jsonSchema.input` / `.output` | live | `S.toJSONSchema(schema)` / `…(S.reverse(schema))` |
| `types.ts` | live | `expectTypeOf` under Vitest typecheck |
| `instantiations` | planned (`_skip: todo`) | `@ark/attest` type-instantiation count |
| `bundleBytes` | planned (`_skip: todo`) | treeshaken per-import-surface size |
| `properties` (property-based testing) | planned (`_skip: todo`) | see below |

### Property-based testing (planned)

PBT is scoped out of the initial harness but is the format's most valuable future
dimension. The `properties` field will name reusable properties — checked against
inputs generated from the schema itself — such as:

- **round-trip**: `encode(decode(x)) == x` for reversible schemas (Sury's core
  reversibility guarantee), skipped as `lossy` for non-injective transforms.
- **reverse-involution**: `reverse(reverse(schema))` behaves like `schema`.
- **no-crash-on-garbage**: random `unknown` inputs either parse or throw a
  well-formed `S.Error` with a correct path — never an internal crash.

Building it requires a `schema → arbitrary` derivation (a generator that produces
values valid by construction) plus per-case opt-out for properties that don't
hold. Until then, every spec must carry `properties: { _skip: todo(#pbt-dimension) }`.

## Make comparison

https://bundlejs.com/

`sury`

```ts
export * as S from "sury@10.0.0-rc.0";
```

```ts
import * as S from "sury@10.0.0-rc.0";

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
S.parseOrThrow(data, schema);
```

valibot

```ts
export * as v from "valibot@1.0.0";
```

```ts
import * as v from "valibot@1.0.0";

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
export * as z from "zod@4.0.0-beta.20250420T053007";
```

```ts
import * as z from "zod@4.0.0-beta.20250420T053007";

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
export * from "@sinclair/typebox";
// Include Value for transforms support
export * from "@sinclair/typebox/value";
export * from "@sinclair/typebox/compiler";
```

```ts
import { Type } from "@sinclair/typebox";
import { TypeCompiler } from "@sinclair/typebox/compiler";

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
export * from "arktype@2.1.20";
```

```ts
import { type } from "arktype@2.1.20";

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

## License

By contributing your code to the rescript-schema GitHub repository, you agree to license your contribution under the MIT license.
