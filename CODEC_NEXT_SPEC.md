# Codec next

Draft spec for the next codec implementation — how a conversion (`S.to`, or the
implicit one created by reversing a schema) picks what to decode into what.

Nothing here is implemented yet. `docs/*-usage.md` still describes the shipped
three-tier algorithm; this file replaces it once the implementation lands. The
`packages/sury/specs/codec-*.yaml` specs snapshot today's behavior and each
carries a `FIXME: Codec next expects:` note on its schema saying what this spec
demands of it.

## Shared definitions

> Two schemas have the **same type** when their type tags match — including the
> class for instances and the format for formatted primitives, where relevant.
> `S.int32` and `S.number` are different types, and so are `S.json` and
> `S.string` — even though every JSON string would validate against `S.string`.
> A schema with no tag of its own to compare — a recursive schema, or a union
> treated as a normal schema (below) — has the same type as another only when
> the two are strictly equal, i.e. the same schema reference.

- **Checks run at operation creation time**, against the **derived** types — the
  actual schema at that point in the pipeline, which may be narrower than the
  originally defined one (an upstream transformation can refine it). Reversing a
  schema doesn't re-run the checks: it reverses the already-resolved per-variant
  pipelines.
- **Nested unions are flattened** before the rules apply:
  `S.union([S.string, S.union([S.number, S.boolean])])` acts as a three-variant
  union. The exception is a union carrying its own format, transformation, or
  refinement — it's treated as a normal (non-union) schema on its side of the
  conversion, so it type-matches by strict equality: the same union reference on
  the other side matches, an identically written one does not.
- **`S.never` marks an unreachable path.** `S.never` variants — including
  transformed ones like `S.never.with(S.to, S.number)`, which match by their
  `never` input — are ignored by type matching: they never trigger the
  exceptions below and don't count toward rule 4's coverage.

## Rule 1: non-union → non-union

Built-in decoding (coercion) always applies:

```ts
S.string.with(S.to, S.schema(undefined)); // "undefined" <-> undefined
S.schema(null).with(S.to, S.schema(undefined)); // null <-> undefined
```

**Literal → literal is one remap, not a table of pairs.** Whatever their tags,
one primitive literal decodes into another by validating the source const and
returning the target const — `null <-> undefined` is an instance of it, not a
nullish special case:

```ts
S.schema("a").with(S.to, S.schema(42)); // "a" <-> 42
S.schema(1).with(S.to, S.schema(true)); // 1 <-> true
S.schema(1n).with(S.to, S.schema("one")); // 1n <-> "one"
S.schema(NaN).with(S.to, S.schema(0)); // NaN <-> 0
```

Collection literals stay out of it — `S.schema([1, 2]).with(S.to,
S.schema("done"))` keeps today's behavior, snapshot in
`codec-literal-array-literal-string`.

## Rule 2: non-union → union

The built-in decoder is applied separately for every target variant, attempted
in definition order:

```ts
const schema = S.json.with(S.to, S.union([S.bigint, S.string]));

S.parser(schema)("123"); // 123n — the bigint variant comes first
S.parser(schema)("abc"); // "abc" — bigint decoding fails, string accepts
S.parser(schema)(true); // throws — no implicit double decoding (true -> "true" -> ...)
```

`S.json` is not the exact `string` type, so string inputs still go through
variant decoding instead of passing through.

**Definition order is not tag order.** The first variant that accepts wins, and
a differently-tagged variant sitting between two same-tag ones keeps its turn.

```ts
const schema = S.json.with(S.to, S.union(["123", S.bigint, S.string]));

S.parser(schema)("123"); // "123" — the literal matches first and stays a string
S.parser(schema)("124"); // 124n — the literal fails, the bigint variant decodes
S.parser(schema)("abc"); // "abc" — bigint decoding fails, the catch-all string accepts
```

**Built-in decoding fills gaps, it doesn't re-type what the source already
has.** A variant whose tag the source can produce takes those values as they
are; the built-in decoder only steps in for a variant whose tag the source has
no way to produce. `bigint` is not a JSON tag, so a JSON string is offered to
`BigInt`; `string` and `number` both are, so a JSON boolean never becomes
`"true"` and a JSON string never becomes a number:

```ts
const schema = S.json.with(S.to, S.union([S.literal("a"), S.number, S.literal("b")]));

S.parser(schema)("b"); // "b" — "a" fails, "b" matches
S.parser(schema)("5"); // throws — S.number takes JSON numbers as they are, it doesn't decode "5"
S.parser(schema)("c"); // throws — no variant accepts
```

**Grouping is codegen, not semantics.** Emitting same-tag variants under one
shared type check — `typeof i==="string"&&(i==="a"||i==="b")` — is an
optimization, allowed exactly while it can't change which variant wins. Hoisting
`"a"` and `"b"` past `S.number` above is legal: neither literal can take a value
`S.number` would have taken. Hoisting `"123"` and the catch-all `S.string` past
`S.bigint` is not: the catch-all takes every string, `"124"` among them. Where
the shortcut isn't available, each check stays in its own definition slot and
the repeated `typeof` is reused from a var.

`S.unknown` is a normal type here — it only matches another `unknown`. An
`unknown` source matches none of the concrete variants, so it takes the same
path: every variant is attempted, in definition order.

**Exception — partial type match.** If the source has the same type as *some but
not all* target variants, the operation is rejected when it's created. Sury
can't tell whether you want a pass-through for the matching variant, decoding
attempts in definition order, or simply widened the type with no decoding
intent:

```ts
S.string.with(S.to, S.union([S.number, S.string]));
// Invalid operation: for "123" — keep "123" or decode to 123?
```

Say what you mean with an explicit variant:

```ts
// Try decoding to number first, keep the string otherwise:
S.string.with(S.to, S.union([S.string.with(S.to, S.number), S.string]));

// Pass strings through, never producing a number:
S.string.with(S.to, S.union([S.never.with(S.to, S.number), S.string]));
```

The same applies to widening into optional or nullable targets:

```ts
S.string.with(S.to, S.optional(S.string));
// Invalid operation: for "undefined" — keep the string or decode to undefined?

// Widen without decoding — the undefined variant is unreachable:
S.string.with(S.to, S.union([S.string, S.never.with(S.to, S.schema(undefined))]));
```

The `Invalid operation` error suggests these rewrites.

## Rule 3: union → non-union

The mirror of rule 2: every source variant gets its own built-in decoder to the
target, dispatched in definition order:

```ts
const schema = S.union([S.bigint, S.string]).with(S.to, S.json);

S.parser(schema)(123n); // "123"
S.parser(schema)("123"); // "123"
S.parser(schema)("abc"); // "abc"
```

**Exception — partial type match.** If the target has the same type as some but
not all source variants, the operation is rejected. Sury can't tell whether the
non-matching variants should decode to the target or be rejected as failed
cases:

```ts
S.union([S.number, S.string]).with(S.to, S.string);
// Invalid operation: for 123 — decode to "123" or fail as a non-matching case?
```

Say what you mean with an explicit target union:

```ts
// Decode numbers to strings:
S.union([S.number, S.string]).with(S.to, S.union([S.number.with(S.to, S.string), S.string]));

// Reject numbers:
S.union([S.number, S.string]).with(S.to, S.union([S.number.with(S.to, S.never), S.string]));
```

The `Invalid operation` error suggests these rewrites.

## Rule 4: union → union

No coercion — values pass through to the same-type target variant. The two
unions must cover each other: every source variant needs at least one same-type
target variant, and every target variant needs at least one same-type source
variant. Otherwise the operation is rejected:

```ts
S.union([S.string, S.number]).with(S.to, S.union([S.number, S.string])); // ✅
S.union([S.string, S.number]).with(S.to, S.union([S.number, S.string, S.boolean])); // ❌ boolean has no source variant
S.union([S.string, S.number, S.boolean]).with(S.to, S.union([S.number, S.string])); // ❌ boolean has no target variant
S.union([S.string, S.number, S.bigint]).with(S.to, S.union([S.json, S.bigint])); // ❌ json is not the exact string/number type
```

A transformed source variant matches by its output type, and a transformed
target variant by its input type — so per-variant conversion is always available
explicitly:

```ts
S.optional(S.string).with(S.to, S.nullable(S.boolean)); // ❌ string doesn't match boolean
S.optional(S.string).with(S.to, S.nullable(S.string.with(S.to, S.boolean))); // ✅
```

**Exception — nullish bridge.** A `null` or `undefined` variant left unmatched
by type may match the opposite nullish variant on the other side — even one that
already has a same-type match. At runtime the same-type target wins; the bridge
only kicks in when there is none:

```ts
S.optional(S.string).with(S.to, S.nullable(S.string)); // ✅ undefined <-> null
S.optional(S.literal("x")).with(S.to, S.nullable(S.literal("x"))); // ✅ "x" matches by type, undefined <-> null
S.optional(S.string).with(S.to, S.union([S.string, null, undefined])); // ✅ undefined -> undefined (same type wins); reverse maps null -> undefined
```

**Worked example** — `S.union([S.bigint, S.number, null]).with(S.to, S.union([S.bigint, S.number, undefined]))`:

Forward:

- `123n` → `123n` (bigint passes through)
- `123.12` → `123.12` (number passes through)
- `null` → `undefined` (nullish bridge)

Reverse (via `S.encoder`):

- `undefined` → `null` (nullish bridge)
- `123n` → `123n`, `123.12` → `123.12` (pass through)

Union conversion always performs exhaustive validation — every variant is
checked, so transformed unions stay consistent across decode and encode.

## Spec coverage

Behavior change expected, today's goldens are wrong:

| Spec                                 | Rule | Expected                                                             |
| ------------------------------------ | ---- | -------------------------------------------------------------------- |
| `codec-union-nested-refined-union`   | —    | a union with its own refinement isn't flattened, and it still refines |
| `codec-json-union2`                  | 2    | non-bigint string falls back to the `S.string` variant               |
| `codec-json-union3-ungrouped`        | 2    | `"123"` matches the literal, `"124"` reaches the `S.bigint` variant  |
| `codec-number-union2-int32`          | 2    | compiles instead of crashing; int32 first, string next               |
| `codec-string-optional-partial`      | 2    | rejected — partial type match                                        |
| `codec-string-union2-partial`        | 2    | rejected — partial type match                                        |
| `codec-unknown-union2`               | 2    | per-variant decoding, same path as `codec-json-union2`               |
| `codec-union2-string-partial`        | 3    | rejected — partial type match                                        |
| `codec-optional-nullable-partial`    | 4    | rejected — `string` has no same-type target variant                  |
| `codec-union2-union3-extra-target`   | 4    | rejected — `boolean` has no source variant                           |
| `codec-union3-union2-extra-source`   | 4    | rejected — `boolean` has no target variant                           |
| `codec-union3-union2-json`           | 4    | rejected — `json` is not the exact `string`/`number` type             |

Already spec-conformant (their remaining `FIXME`s are codegen bugs, not rule
changes): `codec-bool-number-unsupported`, `codec-json-union3-grouped`,
`codec-literal-array-literal-string`, `codec-literal-string-literal-number`,
`codec-null-undefined`, `codec-optional-literal-nullable-literal`,
`codec-optional-nullable-transformed`, `codec-optional-nullable`,
`codec-optional-nullish`, `codec-string-optional-never`, `codec-string-undefined`,
`codec-string-union2-never`, `codec-string-union2-transformed`,
`codec-union-nested-union3-flatten`, `codec-union2-union2-reject`,
`codec-union2-union2-swap`, `codec-union2-union2-transformed`,
`codec-union3-union3-bridge`.
