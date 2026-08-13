# Using Sury with Effect v4

Investigation notes, verified against `effect@4.0.0-rc.108` and Sury `11.0.0-rc.0`.

Effect v4 ships one `effect` package; schemas live in `Schema`, with
`SchemaIssue`, `SchemaGetter`, `SchemaTransformation`, `SchemaRepresentation`
and `JsonSchema` alongside it. Two facts shape every option below:

- Effect **produces** Standard Schema (`Schema.toStandardSchemaV1`,
  `Schema.toStandardJSONSchemaV1`) but never **consumes** one. There is no
  `fromStandardSchemaV1`, so a Sury schema cannot be handed to `HttpApi`, `Rpc`
  or `Schema.Struct` as-is.
- Effect's own extension point for foreign validators is `Schema.declare` plus a
  `SchemaTransformation`, and its serializable format is
  `SchemaRepresentation` / JSON Schema Draft 2020-12.

That gives four integration paths, in order of cost.

## 1. Standard Schema — already works

Anything in the Effect ecosystem that takes a `StandardSchemaV1` (TanStack Form,
Elysia, tRPC, ...) takes a Sury schema today, with no adapter and no dependency
on `effect`:

```ts
const User = S.schema({ id: S.uuid, age: S.number.with(S.gte, 18) });
User["~standard"].validate({ id: "x", age: 42 });
// { issues: [{ message: 'Expected uuid, received "x"', path: ["id"] }] }
```

Two behaviours differ from Effect's implementation of the same interface, and
both are visible to those consumers:

| | Sury | Effect v4 |
| --- | --- | --- |
| async schema | `validate` throws `Encountered unexpected async transform or refine` | returns `Promise<Result>` |
| input failing two fields | first issue only | one issue per field |

The spec allows `Promise<Result>`, and form libraries rely on it — TanStack's
`onChangeAsync` is fed a Standard Schema directly. Sury can already tell
statically whether a schema is async (`isAsyncInternal` in `src/parse.ts`), so
`~standard.validate` could compile the async decoder for those schemas and
return the promise instead of throwing.

Fail-fast is a deliberate Sury trade, but "which fields are wrong" is the whole
point of the form integrations, so an opt-in multi-issue mode is what would
close the second row.

## 2. `fromSury` — a Sury codec as an Effect `Schema.Codec`

`Schema.declare` on both sides of a `SchemaTransformation` lets Sury's compiled
decoder do all validation and conversion, while Effect keeps the types, the
issue tree and the composition:

```ts
import { Effect, Schema, SchemaGetter, SchemaIssue, SchemaTransformation } from "effect";
import * as S from "sury";

const isSuryError = (e: unknown): e is S.Error =>
  e instanceof (S.Error as unknown as new () => S.Error);

// Sury's `error.path` is a string (`["a"]["0"]`); the array form is internal.
const pathToArray = (path: string): ReadonlyArray<PropertyKey> => {
  const out: string[] = [];
  const re = /\["((?:[^"\\]|\\.)*)"\]/g;
  let m: RegExpExecArray | null;
  while ((m = re.exec(path))) out.push(m[1].replace(/\\(.)/g, "$1"));
  return out;
};

const toIssue = (exn: unknown): SchemaIssue.Issue => {
  if (!isSuryError(exn)) throw exn;
  const leaf = new SchemaIssue.InvalidValue({ message: exn.reason });
  const path = pathToArray(exn.path as unknown as string);
  return path.length ? new SchemaIssue.Pointer(path, leaf) : leaf;
};

// No Effect.suspend around the try/catch: the getter is already called lazily,
// and the extra suspend costs ~40% of the adapter's throughput.
const attempt = <A>(f: () => A): Effect.Effect<A, SchemaIssue.Issue> => {
  try {
    return Effect.succeed(f());
  } catch (exn) {
    return Effect.fail(toIssue(exn));
  }
};

export const fromSury = <I, O>(sury: S.Schema<I, O>): Schema.Codec<O, I> => {
  const decode = S.decoder(sury);
  const encode = S.encoder(sury);
  // `() => true` on both sides: Sury has already validated, so Effect must not
  // spend a second guard on the same value.
  const To = Schema.declare((_u): _u is O => true);
  const From = Schema.declare((_u): _u is I => true);
  return To.pipe(
    Schema.encodeTo(
      From,
      SchemaTransformation.make({
        decode: SchemaGetter.transformOrFail((i: I) => attempt(() => decode(i))),
        encode: SchemaGetter.transformOrFail((o: O) => attempt(() => encode(o))),
      })
    )
  ) as unknown as Schema.Codec<O, I>;
};
```

Sury's Input maps to Effect's `Encoded` and Sury's Output to Effect's `Type`, so
`decodeUnknownSync` runs `S.decoder` and `encodeUnknownResult` runs `S.encoder`.
The result composes: dropped into `Schema.Struct({ user: User })`, a failure
inside the Sury schema reports as `path: ["user", "id"]`, because the adapter's
`Pointer` nests under Effect's. `Schema.toCodecJson` works on it. An async Sury
schema needs the same adapter over `S.asyncDecoder` / `S.asyncEncoder` wrapped in
`Effect.tryPromise`.

### Performance

The reason to do this at all. Decoding an array of `{ id: uuid, age: int
18..100, tags: string[] }`, ops/s, Node 22:

| payload | Effect native | Effect + `fromSury` | Sury raw |
| --- | --- | --- | --- |
| 1 object | 532k | 951k | 7.6M |
| 10 objects | 91k | 492k | 906k |
| 100 objects | 9.4k | 100k | 92k |
| 1000 objects | 936 | 11.0k | 9.1k |

Effect's fixed per-call cost is ~1 µs and does not shrink: an identity function
through the same declaration + transformation runs at 1.1M ops/s, a bare
`Schema.declare` decode at 1.5M. So the adapter cannot deliver Sury's 7.6M ops/s
for a one-object payload — it delivers 1.8×. From ~10 fields upward the Sury
side dominates and the adapter is worth 5–12× over native Effect Schema,
converging on raw Sury throughput.

### Limitation

`Schema.toJsonSchemaDocument` on a `declare` node returns `{}` — silently, no
error. Anything that derives a document from the schema (HttpApi's OpenAPI
output, AI tool definitions) gets an empty schema for that node. Path 3 is the
fix.

## 3. Hybrid — Sury parses, Effect describes

Rebuild the encoded side as a real Effect schema from Sury's own JSON Schema, and
keep Sury as the decoder:

```ts
const From = SchemaRepresentation.fromJsonSchemaDocument({
  dialect: "draft-2020-12",
  schema: S.toJSONSchema(sury, { target: "draft-2020-12" }),
  definitions: {},
});
const To = Schema.declare((_u): _u is O => true);
// ... same SchemaTransformation as above
```

`Schema.toJsonSchemaDocument` then emits the real structure:

```json
{"type":"object","properties":{"id":{"type":"string","format":"uuid"},
 "age":{"type":"number","allOf":[{"minimum":18}]},"name":{"type":"string"}},
 "required":["id","age"]}
```

The cost is that Effect validates the encoded side before Sury validates it
again. Whether that is acceptable depends on the endpoint; the two-node split is
what makes the OpenAPI document honest.

## 4. JSON Schema as a translation bridge

Both libraries convert to and from JSON Schema Draft 2020-12, so schemas — not
just values — can cross:

- Effect → Sury: `S.fromJSONSchema(Schema.toJsonSchemaDocument(effectSchema).schema)`
- Sury → Effect: `SchemaRepresentation.fromJsonSchemaDocument({ dialect: "draft-2020-12", schema: S.toJSONSchema(surySchema, { target: "draft-2020-12" }), definitions: {} })`

Both directions run and both produce working validators. What is lost:

- **Transformations, both ways.** Effect states this for
  `SchemaRepresentation` ("the representation format describes the schema's
  shape"), and JSON Schema has nowhere to put a decode function anyway. An
  Effect `Schema.Date` decoded from a string arrives in Sury as `string`.
- **Effect's checks arrive as `allOf`.** `Schema.String.check(Schema.isUUID())`
  emits `{"type":"string","allOf":[{"pattern":"...","format":"uuid"}]}`. Sury
  honours it at runtime, but the failure reads `Should pass for all schemas of
  the allOf property` instead of naming the constraint, and re-emitting the
  rebuilt schema with `S.toJSONSchema` drops the `allOf` branch — a fidelity gap
  worth a spec of its own.
- **`format` is advisory on the Effect side.** A Sury `S.uuid` crosses as
  `{"type":"string","format":"uuid"}` and the rebuilt Effect schema accepts
  `"x"`.

So this path fits code generation and contract sharing, not silent substitution
of one library for the other.

## What would make this smoother in Sury

1. `~standard.validate` should return a `Promise` for async schemas instead of
   throwing — the spec allows it and the form integrations depend on it.
2. An opt-in multi-issue mode for `~standard.validate`.
3. Export the path-to-array helper. It exists as `$pathToArray` for ReScript
   only, so every JS adapter author re-parses the `["a"]["0"]` string, as above.
4. `S.fromJSONSchema` → `S.toJSONSchema` should round-trip `allOf` constraints it
   already enforces.

None of these is Effect-specific; they are what any Standard Schema consumer or
adapter author hits first.

## Reproducing

The measurements above come from a scratch package with `effect@rc` and this
repo's `packages/sury` linked in:

```bash
pnpm --filter=sury build:entry   # produces packages/sury/index.mjs
npm i effect@rc tsx typescript
ln -s <repo>/packages/sury node_modules/sury
```

then the `fromSury` adapter from section 2 with `Schema.decodeUnknownSync`
against `Schema.decodeUnknownSync` of the equivalent `Schema.Struct`.
