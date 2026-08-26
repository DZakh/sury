[![CI](https://github.com/DZakh/sury/actions/workflows/ci.yml/badge.svg)](https://github.com/DZakh/sury/actions/workflows/ci.yml)
[![sury npm](https://img.shields.io/npm/dm/sury?label=Sury)](https://www.npmjs.com/package/sury)
[![rescript-schema npm](https://img.shields.io/npm/dm/rescript-schema?label=ReScript%20Schema)](https://www.npmjs.com/package/rescript-schema)
[![license](https://img.shields.io/npm/l/sury)](https://github.com/DZakh/sury/blob/main/LICENSE)

# Sury 🧬

**The batteries-included schema library for TypeScript and ReScript.**

Declare your data once, as a pipeline — wire format on one side, the types you work with on the other. Sury derives the decoder, the encoder, and the JSON Schema from that one declaration, and JIT-specializes each into a function written for exactly your shape.

- **Declarative pipelines, both directions.** `S.jsonString.with(S.to, eventSchema)` decodes in and encodes out — no second schema for the wire format, and [JSON encoding faster than `JSON.stringify`](#json-serialization-faster-than-jsonstringify).
- **JIT-specialized, not interpreted.** The fastest parsing in the ecosystem — [see the code a schema turns into](#the-code-a-schema-turns-into) and the [benchmarks](#comparison). It runs on `new Function`: [here's why you shouldn't worry](#does-it-really-use-new-function).
- **Batteries included.** [JSON Schema in and out](#json-schema-in-and-out) — `$ref`, recursion, no codegen, no `any` — plus the [string formats](https://github.com/DZakh/sury/blob/main/docs/js-usage.md#string-formats), async and custom schemas, in 7.9 kB min+gzip for a schema and a parser, tree-shakable.

## Getting started

```sh
npm install sury
```

```ts
import * as S from "sury";

const playerSchema = S.schema({
  username: S.string,
  xp: S.number,
});

// Create the function once, call it as many times as you like
const parsePlayer = S.parser(playerSchema);

parsePlayer({ username: "billie", xp: 100 });
// => { username: "billie", xp: 100 }

parsePlayer({ username: "billie", xp: "not a number" });
// => throws S.Error: Failed at ["xp"]: Expected number, received "not a number"

type Player = S.Infer<typeof playerSchema>;
//   ^? { username: string; xp: number }
```

The API mirrors TypeScript types, so there's not much new syntax to learn. If you'd rather get a result back than catch, there's [`S.safe`](#errors-that-tell-you-where-to-look).

**Full API reference:** [JS/TS](https://github.com/DZakh/sury/blob/main/docs/js-usage.md) · [ReScript](https://github.com/DZakh/sury/blob/main/docs/rescript-usage.md) · [PPX](https://github.com/DZakh/sury/blob/main/packages/sury-ppx/README.md)

> Formerly published as **rescript-schema**. Sury is plain JavaScript — the ReScript compiler is not involved — with first-class ReScript bindings on the same package.

## Why Sury

### Discriminated unions, decoded straight from a JSON string

Declare the union once, and get parsing and narrowing from it:

```ts
const eventSchema = S.union([
  { type: "user.created", id: S.bigint },
  { type: "user.renamed", id: S.bigint, name: S.string },
  { type: "user.deleted", id: S.bigint },
]);

// Chain schemas to build a pipeline — no JSON.parse in your own code
const parseEvent = S.decoder(S.jsonString, eventSchema);

const event = parseEvent('{"type":"user.renamed","id":"42","name":"Dmitry"}');
// => { type: "user.renamed", id: 42n, name: "Dmitry" }

switch (event.type) {
  case "user.renamed":
    event.name; // string — TypeScript narrows it for you
    break;
}
```

`S.decoder(from, to)` creates the function that converts between two schemas — `S.parser(schema)` from the previous example is just `S.decoder(S.unknown, schema)`.

You write `id: S.bigint` — the type you want to work with. A `bigint` can't exist in JSON, so Sury infers the `"42"` → `42n` coercion from the input side of the pipeline, in both directions. No `as const`, no coercion wrappers, no second schema for the wire format. And the coercion belongs to this chain, not to `S.bigint`: a plain `S.parser(eventSchema)` expects a real `bigint` and rejects `"42"`.

Errors point at the field inside the matched variant, not at the union as a whole:

```ts
parseEvent('{"type":"user.renamed","id":"42"}');
// => throws S.Error: Failed at ["name"]: Expected string, received undefined
```

### The code a schema turns into

`parseEvent` isn't an interpreter walking a schema tree. It's a function Sury specialized for exactly this shape. The union dispatches on the discriminant, the inferred `bigint` coercion is inlined as a bare `BigInt()` call, and `S.jsonString` → union → fields fuse into one pass:

```js
(i) => {
  let v0;
  try {
    v0 = JSON.parse(i);
  } catch (t) {
    e[0](i);
  }
  if (typeof v0 === "object" && v0 && !Array.isArray(v0)) {
    if (v0["type"] === "user.created") {
      let v2 = v0["id"];
      typeof v2 === "string" || e[2](v2);
      let v1;
      try {
        v1 = BigInt(v2);
      } catch (_) {
        e[1](v2);
      }
      v0 = { type: v0["type"], id: v1 };
    } else if (v0["type"] === "user.renamed") {
      // ...one branch per variant, no loop over union members
    } else {
      e[8](v0);
    }
  } else {
    e[9](v0);
  }
  return v0;
};
```

That's why Sury tends to outrun not just other libraries, but hand-rolled validation too.

### JSON serialization faster than `JSON.stringify`

The same `eventSchema` encodes back out. No second definition:

```ts
S.encoder(eventSchema, S.jsonString)({ type: "user.renamed", id: 42n, name: "Dmitry" });
// => '{"type":"user.renamed","id":"42","name":"Dmitry"}'
```

There's no intermediate object and no `JSON.stringify` — the discriminant picks a branch, and the JSON text is baked in:

```js
(i) => {
  for (;;) {
    if (typeof i === "object" && i && i["type"] === "user.renamed") {
      i = '{"type":"user.renamed","id":"' + i["id"] + '","name":' + e[0](i["name"]) + "}";
      break;
    }
    // ...one branch per variant
  }
  return i;
};
```

Types `JSON.stringify` refuses are ordinary fields here. Values it silently corrupts throw instead:

```ts
const schema = S.schema({ id: S.bigint, payload: S.uint8Array, at: S.date, price: S.number });
const encode = S.encoder(schema, S.jsonString);
const bytes = new TextEncoder().encode("hello");

encode({ id: 9007199254740993n, payload: bytes, at: new Date("2026-01-15T10:30:00.000Z"), price: 9.99 });
// => '{"id":"9007199254740993","payload":"hello","at":"2026-01-15T10:30:00.000Z","price":9.99}'

encode({ id: 1n, payload: bytes, at: new Date(), price: Infinity });
// => throws S.Error: Failed at ["price"]: Expected JSON, received Infinity

JSON.stringify({ price: Infinity });
// => '{"price":null}'
```

| Encode to JSON string                        | **Sury**    | `JSON.stringify` | fast-json-stringify |
| -------------------------------------------- | ----------- | ---------------- | ------------------- |
| API response (user profile, 7 fields)        | **305 ns**  | 590 ns           | 382 ns              |
| Event feed (50 tagged-union events)          | **5.05 µs** | 7.82 µs          | 20.26 µs            |
| `bigint` id + binary payload + `Date`        | **1.17 µs** | 1.51 µs          | 1.45 µs             |

Faster than `JSON.stringify`, and 3.5× lighter than fast-json-stringify — 16.4 kB against 56.7 kB, encoder included.

### Chain schemas into pipelines, get the inverse for free

`S.jsonString` above is an ordinary schema used as a stage — so are `S.json`, `S.uint8Array`, `S.date`, and every schema you write. There's no fixed menu of `parseJson` / `parseJsonString` / `convertToJson` functions: you describe the data at each step, and Sury fills in the code between them.

Inside a schema, a stage chains with `schema.with(S.to, target)` — `.with` applies an operation to a schema and returns a new schema. Stages nest, so any field can be its own pipeline:

```ts
const apiUser = S.schema({
  // Arrives as JSON text, parsed and validated as an array of addresses
  addresses: S.jsonString.with(S.to, S.array(addressSchema)),
  // Arrives as a string, mapped to a Date
  createdAt: S.string.with(S.to, S.date),
  // Element-level transforms work the same way
  ids: S.array(S.string.with(S.to, S.bigint)),
});
```

The whole tree still folds into one specialized function — no per-stage overhead.

And the whole tree runs backwards, too. Rename fields with `S.shape`, coerce with `S.to`, and the inverse is derived automatically:

```ts
const userSchema = S.schema({
  USER_ID: S.string.with(S.to, S.bigint),
  USER_NAME: S.string,
}).with(S.shape, (input) => ({
  id: input.USER_ID,
  name: input.USER_NAME,
}));
//? S.Schema<{ USER_ID: string; USER_NAME: string }, { id: bigint; name: string }>

S.parser(userSchema)({ USER_ID: "0", USER_NAME: "Dmitry" });
// => { id: 0n, name: "Dmitry" }

S.encoder(userSchema)({ id: 0n, name: "Dmitry" });
// => { USER_ID: "0", USER_NAME: "Dmitry" }

S.reverse(userSchema);
//? S.Schema<{ id: bigint; name: string }, { USER_ID: string; USER_NAME: string }>
```

`S.reverse` hands you the backwards schema as a real schema — usable with every operation, not just as an encode shortcut.

### JSON Schema, in and out

Sury speaks JSON Schema natively — no converter bolted on top. It goes through the [Standard JSON Schema](https://standardschema.dev/json-schema) extension of the [Standard Schema](https://standardschema.dev/) spec, so tools consume it without special-casing Sury.

And because Sury tracks Input and Output separately, it describes both sides of a transformation:

```ts
// Opt-in, so bundles that don't emit JSON Schema tree-shake it away
S.enableStandardJSONSchema();

const productSchema = S.schema({
  id: S.string,
  price: S.string.with(S.to, S.number),
}).with(S.meta, {
  description: "A product in the catalog",
  examples: [{ id: "p_1", price: 9.99 }],
});

productSchema["~standard"].jsonSchema.input({ target: "draft-2020-12" });
// {
//   $schema: "https://json-schema.org/draft/2020-12/schema",
//   type: "object",
//   properties: { id: { type: "string" }, price: { type: "string" } },
//   required: ["id", "price"],                    ↑ the wire format
//   description: "A product in the catalog",
//   examples: [{ id: "p_1", price: "9.99" }],
// }

productSchema["~standard"].jsonSchema.output({ target: "draft-2020-12" });
// { ... properties: { id: { type: "string" }, price: { type: "number" } }, ... }
//                                                   ↑ what your code receives
```

`S.meta` attaches `description`, `title`, `examples` and `deprecated`. Write examples in the Output format you work with (`price: 9.99`). They're emitted in the Input format the wire uses (`price: "9.99"`), so a generated OpenAPI document describes what a client really sends.

`"draft-07"`, `"draft-2020-12"` and `"openapi-3.0"` are all supported targets, and `S.toJSONSchema(schema, options)` skips `~standard` if you'd rather.

It reads JSON Schema back in too — the whole document, typed as it goes:

```ts
const comment = S.fromJSONSchema({
  $ref: "#/$defs/comment",
  $defs: {
    comment: {
      type: "object",
      properties: {
        text: { type: "string" },
        replies: { type: "array", items: { $ref: "#/$defs/comment" } },
      },
      required: ["text"],
    },
  },
});
//? S.Schema<{ text: string; replies?: ...[] | undefined }>

S.assert(comment, { text: "hi", replies: [{ text: 1 }] });
// => throws S.Error: Failed at ["replies"]["0"]["text"]: Expected string, received 1
```

### Types you can actually read

Hover any schema and you see the data, not the library's internals:

```ts
S.schema({ foo: S.string });
//? S.Schema<{ foo: string }, { foo: string }>
```

Compare that with `v.ObjectSchema<{readonly foo: v.StringSchema<undefined>}, undefined>`. Both sides are right there, and they read in the direction the data flows — `S.Schema<TInput, TOutput>`.

### Errors that tell you where to look

```ts
S.parser(S.schema({ a: S.array(S.schema({ b: S.string })) }))({
  a: [{ b: "x" }, { b: 1 }],
});
// => throws S.Error: Failed at ["a"]["1"]["b"]: Expected string, received 1
```

Every error is an `S.Error` (`err instanceof S.Error`). If you'd rather not catch, `S.safe` and `S.safeAsync` wrap any block into a typed result:

```ts
const result = S.safe(() => S.parser(playerSchema)(data));
if (result.success) result.value;
else result.error;
```

## Recipes

Scenarios that usually need a helper library or hand-written glue, as plain schemas.

### Decode a JWT payload

A JWT segment is base64url text holding JSON. Declare the layers and get both directions:

```ts
const claimsSchema = S.base64url.with(
  S.to,
  S.jsonString.with(S.to, S.schema({ sub: S.string, exp: S.number })),
);

S.parser(claimsSchema)("eyJzdWIiOiJhIiwiZXhwIjoxNzM1Njg2MDAwfQ");
// => { sub: "a", exp: 1735686000 }
```

### Read an uploaded config file

A `File`'s content is only readable asynchronously, so the pipeline is async as a whole — and the sync parser refuses at creation instead of failing later:

```ts
const configSchema = S.file.with(
  S.to,
  S.jsonString.with(S.to, S.schema({ theme: S.string })),
);

await S.asyncParser(configSchema)(new File(['{"theme":"dark"}'], "config.json"));
// => { theme: "dark" }

S.parser(configSchema);
// => throws: Invalid async during sync operation
```

### Type your environment variables

`process.env` is strings; your config isn't. Pipe from `S.record(S.string)` and the coercions are inferred — extra variables pass through untouched:

```ts
const envSchema = S.record(S.string).with(
  S.to,
  S.schema({
    PORT: S.port,
    DEBUG: S.string.with(S.to, S.boolean),
  }),
);

S.parser(envSchema)(process.env);
// => { PORT: 8080, DEBUG: true }

S.parser(envSchema)({ PORT: "99999", DEBUG: "true" });
// => throws S.Error: Failed at ["PORT"]: Expected port, received 99999
```

### ISO strings ↔ `Date`

```ts
const at = S.string.with(S.to, S.date);

S.parser(at)("2026-08-26T12:00:00.000Z"); // => Date
S.encoder(at)(new Date("2026-08-26T12:00:00.000Z")); // => "2026-08-26T12:00:00.000Z"
```

### Columnar data to rows

`S.compactColumns` maps columnar arrays — of `S.json` values here — to rows, in both directions:

```ts
const cityRow = S.schema({ id: S.bigint, city: S.string });
const rows = S.compactColumns(S.json).with(S.to, S.array(cityRow));

S.parser(rows)([["1", "2"], ["Tbilisi", "Batumi"]]);
// => [{ id: 1n, city: "Tbilisi" }, { id: 2n, city: "Batumi" }]

S.encoder(rows)([{ id: 1n, city: "Tbilisi" }, { id: 2n, city: "Batumi" }]);
// => [["1", "2"], ["Tbilisi", "Batumi"]]
```

More building blocks in the [API reference](https://github.com/DZakh/sury/blob/main/docs/js-usage.md).

## Comparison

Sury is the fastest composable validation library in the ecosystem, because schemas are JIT-specialized with `new Function` rather than interpreted.

It's also small. Instead of a few large classes with many methods, the API and source are built from many small, independent functions, each with a single task. A bundler follows your imports and drops everything you don't use, which can cut the shipped size by up to 2× compared to [Zod](https://github.com/colinhacks/zod). (The approach is borrowed from [Valibot](https://github.com/fabian-hiller/valibot), which pioneered it.)

Measured with [this repo's comparison benchmark](https://github.com/DZakh/sury/tree/main/packages/e2e/src/benchmark) against `sury@11.0.0-rc.1`, `zod@4.4.3`, `typebox@0.34.52`, `valibot@1.4.2`, `arktype@2.2.3`:

### Size & speed

|                                 | Sury           | Zod          | TypeBox                        | Valibot      | ArkType        |
| ------------------------------- | -------------- | ------------ | ------------------------------ | ------------ | -------------- |
| **Total size** (min + gzip)     | 31.5 kB        | 65.0 kB      | 31.3 kB                        | 15.3 kB      | 47.2 kB        |
| **Benchmark size** (min + gzip) | 8.0 kB         | 19.6 kB      | 22.6 kB                        | 1.29 kB      | 47.1 kB        |
| **Parse with the same schema**  | 210,061 ops/ms | 9,367 ops/ms | 158,185 ops/ms (no transforms) | 1,970 ops/ms | 106,520 ops/ms |
| **Create schema & parse once**  | 99 ops/ms      | 11 ops/ms    | 103 ops/ms (no transforms)     | 315 ops/ms   | 11 ops/ms      |

"Total size" is the whole library; "Benchmark size" is what the benchmark's schema and parse pull into a bundle after tree-shaking. The TypeBox numbers come from its compiled validator, which checks the value but doesn't transform or rebuild it.

Independent benchmarks and conformance suites that include Sury:

- [typescript-runtime-type-benchmarks](https://moltar.github.io/typescript-runtime-type-benchmarks/) — throughput across the ecosystem
- [schemabenchmarks.dev](https://schemabenchmarks.dev/) — per-step breakdown: download, initialization, validation, parsing, Standard Schema, codec
- [json-schema-compliance-suite](https://github.com/sinclairzx81/json-schema-compliance-suite) — JSON Schema validation, semantics, and round-trip fidelity

### Features

|                                          | Sury                                     | Zod                                       | TypeBox                   | Valibot                                                               | ArkType                   |
| ---------------------------------------- | ---------------------------------------- | ----------------------------------------- | ------------------------- | --------------------------------------------------------------------- | ------------------------- |
| **Inferred TS type** (what you hover)    | `S.Schema<{foo: string}, {foo: string}>` | `z.ZodObject<{foo: z.ZodString}, $strip>` | `TObject<{foo: TString}>` | `v.ObjectSchema<{readonly foo: v.StringSchema<undefined>}, undefined>` | `Type<{foo: string}, {}>` |
| **JSON Schema**                          | `S.toJSONSchema` + `S.fromJSONSchema`    | `z.toJSONSchema`                          | 👑                        | `@valibot/to-json-schema`                                             | `myType.toJsonSchema()`   |
| **Standard Schema**                      | ✅                                       | ✅                                        | ❌                        | ✅                                                                    | ✅                        |
| **Codegen-free** (doesn't need compiler) | ✅                                       | ✅                                        | ✅                        | ✅                                                                    | ✅                        |
| **Eval-free**                            | ❌                                       | ⭕ opt-out                                | ⭕ opt-in                 | ✅                                                                    | ⭕ opt-out                |
| **Ecosystem**                            | ⭐️⭐️                                   | ⭐️⭐️⭐️⭐️⭐️                           | ⭐️⭐️⭐️⭐️⭐️           | ⭐️⭐️⭐️                                                             | ⭐️⭐️                    |

Sury's own ecosystem is young, but implementing Standard Schema means the 32+ libraries that support the spec already work with it today.

## Integrations

Use Sury anywhere a schema is accepted:

- [tRPC](https://trpc.io/), [TanStack Form](https://tanstack.com/form), [TanStack Router](https://tanstack.com/router), [Hono](https://hono.dev/), and 28+ more via the [Standard Schema](https://standardschema.dev/) spec
- Anything that speaks [JSON Schema](https://json-schema.org/), via `S.toJSONSchema` / `S.fromJSONSchema`

## Used by

- [HyperIndex](https://github.com/enviodev/hyperindex) — Envio's blockchain indexing framework, which uses Sury to power native high-performance external calls
- [rescript-rest](https://github.com/DZakh/rescript-rest) — RPC-like client, contract, and server implementation for a pure REST API
- [rescript-envsafe](https://github.com/DZakh/rescript-envsafe) — makes sure you don't accidentally deploy apps with missing or invalid environment variables
- [rescript-stripe](https://github.com/enviodev/rescript-stripe) — describe and manage Stripe billing in a declarative way with code
- Internal form library at [Carla](https://www.carla.se/)

Building something with Sury? [Let me know](https://x.com/dzakh_dev) and I'll add it here.

## FAQ

### Does it really use `new Function`?

Yes — that's where the speed comes from. It's also how TypeBox, Zod v4 and ArkType work, and even Cloudflare Workers added support for it.

There's currently no eval-free mode, so Sury won't run where dynamic code evaluation is forbidden: pages under a strict CSP without `'unsafe-eval'`, some browser extension contexts, and a few restricted edge runtimes. If that's your environment, [Valibot](https://valibot.dev/) is the honest recommendation today.

### Why "Sury"?

It's short, it's pronounceable, and the 🧬 fits: a schema is the DNA of your data — one definition that everything else is derived from.

## Resources

- Welcome Sury - The fastest schema with next-gen DX ([Dev.to](https://dev.to/dzakh/welcome-sury-the-fastest-schema-with-next-gen-dx-5gl4))
- ReScript Schema unique features ([Dev.to](https://dev.to/dzakh/javascript-schema-library-from-the-future-5420))
- Building and consuming REST API in ReScript with rescript-rest and Fastify ([YouTube](https://youtu.be/37FY6a-zY20?si=72zT8Gecs5vmDPlD))

## Contributing

Bug reports, ideas, and pull requests are all welcome — open an [issue](https://github.com/DZakh/sury/issues) to get started.

## Sponsorship

If you're enjoying Sury and want to give back, that would be rad!

The free ways help a lot too: star the repo, write about it, or tell someone who's picking a validation library this week.

If you'd like to donate, GitHub Sponsors isn't available in my country, so **USDT** is the easiest route:

- ERC20: `0x509fCF7C24A94a776eb92B56B9DA4aA145615529`
- TRC20: `TFg5hKgkdcrFnPHNgYqfbp9yMyx25uaWrF`

Your sponsorship doesn't go towards anything specific – it's simply a wonderful way to say "thank you" and make me happy. 😁

DM me on [X/Twitter](https://x.com/dzakh_dev) if you want to be featured or just to say hi! This would mean so much to me. ✨

## License

[MIT](https://github.com/DZakh/sury/blob/main/LICENSE)
