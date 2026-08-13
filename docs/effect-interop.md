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

That gives three integration paths, in order of cost.

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

## 2. `toEffectSchema` — a Sury codec as an Effect `Schema.Codec`

`Schema.declare` on both sides of a `SchemaTransformation` lets Sury's compiled
decoder do all validation and conversion, while Effect keeps the types, the
issue tree and the composition. Three details make the difference between a
prototype and something usable: a `toCodecJson` annotation so the node still
documents itself, async chosen once at construction, and no `Effect.suspend` on
the hot path.

```ts
import { Effect, Schema, SchemaGetter, SchemaIssue, SchemaRepresentation, SchemaTransformation } from "effect";
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

// No Effect.suspend around the try/catch: the getter is already invoked lazily,
// and the extra suspend costs ~40% of the adapter's throughput.
const getter = <A, B>(
  build: () => (a: A) => B,
  buildAsync: () => (a: A) => Promise<B>
): SchemaGetter.Getter<B, A> => {
  let run: (a: A) => B;
  try {
    run = build();
  } catch {
    // Sury rejects an async schema when the sync operation is built, not when
    // it runs, so this is the only place the choice can be made once.
    const runAsync = buildAsync();
    return SchemaGetter.transformOrFail((a: A) =>
      Effect.tryPromise({ try: () => runAsync(a), catch: toIssue }) as Effect.Effect<B, SchemaIssue.Issue>
    );
  }
  return SchemaGetter.transformOrFail((a: A) => {
    try {
      return Effect.succeed(run(a));
    } catch (exn) {
      return Effect.fail(toIssue(exn));
    }
  });
};

export const toEffectSchema = <I, O>(sury: S.Schema<I, O>): Schema.Codec<O, I> => {
  // `() => true` on both sides: Sury has already validated, so Effect must not
  // spend a second guard on the same value.
  const To = Schema.declare((_u): _u is O => true);
  const From = Schema.declare((_u): _u is I => true, {
    // JSON Schema generation runs the AST through the JSON codec first, so this
    // link is what keeps a declaration from documenting itself as `{}`. It is
    // never consulted while decoding.
    toCodecJson: () =>
      Schema.link<I>()(
        SchemaRepresentation.fromJsonSchemaDocument({
          dialect: "draft-2020-12",
          // Sury's dialect interfaces mirror the frozen specs, so they carry no
          // index signature and don't structurally satisfy Effect's JsonSchema.
          schema: S.toJSONSchema(sury, { target: "draft-2020-12" }) as Record<string, unknown>,
          definitions: {},
        }),
        SchemaTransformation.passthrough<I, unknown>({ strict: false })
      ),
  });
  return To.pipe(
    Schema.encodeTo(
      From,
      SchemaTransformation.make({
        decode: getter<I, O>(() => S.decoder(sury), () => S.asyncDecoder(sury)),
        encode: getter<O, I>(() => S.encoder(sury), () => S.asyncEncoder(sury)),
      })
    )
  );
};
```

That single cast on `S.toJSONSchema`'s result is the only one left; the return
type is satisfied structurally, so nothing at the call site is cast:

```ts
const User = toEffectSchema(S.schema({ id: S.uuid, age: S.number.with(S.gte, 18) }));
// User.Type    = { id: string; age: number }
// User.Encoded = { id: string; age: number }
Schema.decodeUnknownResult(User)({ id: "x", age: 42 });
// issues: [{ path: ["id"], message: 'Expected uuid, received "x"' }]
```

Sury's Input maps to Effect's `Encoded` and Sury's Output to Effect's `Type`, so
`decodeUnknownSync` runs `S.decoder` and `encodeUnknownResult` runs `S.encoder`.
What follows from that:

- **It composes.** Dropped into `Schema.Struct({ user: User })`, a failure inside
  the Sury schema reports as `path: ["user", "id"]`, because the adapter's
  `Pointer` nests under Effect's.
- **It documents itself.** `Schema.toJsonSchemaDocument` on the wrapper — or on
  any struct containing it — emits Sury's own JSON Schema, so HttpApi's OpenAPI
  output and AI tool definitions stay honest. Without the `toCodecJson`
  annotation a `declare` node returns `{}` silently. For a codec the document
  describes the wire side, which is what an endpoint wants: a Sury
  `string → Date` appears as `{"type":"string"}`.
- **Async needs no second entry point.** `S.decoder` throws while *building* the
  operation for an async schema, so one try/catch at construction picks
  `S.asyncDecoder` and the schema is usable from `decodeUnknownEffect` with the
  promise awaited inside the Effect.

### Performance

The reason to do this at all. Decoding an array of `{ id: uuid, age: int
18..100, tags: string[] }`, ops/s, Node 22, same process per row:

| payload | Effect native | Effect + `toEffectSchema` | Sury raw |
| --- | --- | --- | --- |
| 1 object | 657k | 1.22M | 8.7M |
| 10 objects | 125k | 597k | 1.02M |
| 100 objects | 13.4k | 99k | 92k |
| 1000 objects | 1.33k | 11.6k | 9.5k |

Effect's fixed per-call cost is ~1 µs and does not shrink: an identity function
through the same declaration + transformation runs at 1.1M ops/s, a bare
`Schema.declare` decode at 1.5M. So the adapter cannot deliver Sury's 8.7M ops/s
on a one-object payload — it delivers ~1.9×. From ~10 objects up the Sury side
dominates and the adapter is worth 5–9× over native Effect Schema, converging on
raw Sury throughput. (Rows at 100 and 1000 show the adapter level with or above
`sury raw`; that gap is inside run-to-run noise.)

### Where it would live

Not in `packages/sury`. It needs `effect` as a peer dependency, and `entry.ts` is
the single public entry, so an interop entry is a deliberate exception rather
than a detail — either a `sury/effect` subpath (second esbuild entry, its own
line in `artifact_test.ts`'s `FILES`, an optional peer dep) or a standalone
package that depends on both. A standalone package keeps the core artifact and
its bundle-size budget untouched, which is the tiebreak the repo's goals imply.

## 3. JSON Schema as a translation bridge

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
5. The dialect interfaces in `src/types/jsonschema.d.ts` have no index
   signature, so a `S.toJSONSchema` result needs a cast before any consumer
   typed as `Record<string, unknown>` accepts it. Mirroring the frozen specs is
   the point of those interfaces, so this may just be the adapter's job to
   absorb — worth deciding once rather than per adapter.

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

then the `toEffectSchema` adapter from section 2 with `Schema.decodeUnknownSync`
against `Schema.decodeUnknownSync` of the equivalent `Schema.Struct`.
